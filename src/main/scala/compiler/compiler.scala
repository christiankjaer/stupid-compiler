package compiler

import cats.syntax.all.*
import cats.data.{EitherT, State}
import syntax.*

val wordSize = 8
val charShift = 8
val charTag = 0x0f

val fixShift = 2
val fixMask = 0x3
val fixTag = 0x0

val falseVal = 0x2f
val trueVal = 0x6f
val nullVal = 0x3f

val boolShift = 6
val boolMask = 0xbf
val boolTag = 0x2f

enum Binding {
  case StackPos(i: Int)
  case ProgramLabel(l: Label)
}

type Instruction = String
type Label = String
type Env = Map[Name, Binding]
type Error = String

type LabelCounter = Int

// Compile monad
type C[T] = EitherT[State[LabelCounter, _], Error, T]

def error[T](e: Error): C[T] =
  EitherT.leftT(e)

def pure[T](a: T): C[T] = EitherT.pure(a)

val prelude =
  List(
    "    .text",
    "    .globl    program_entry",
    "    .type     program_entry, @function"
  )

val entry = List(
  "program_entry:",
  s"    movq %rsp, %rcx", // Save original stack
  s"    leaq (%rdi, %rsi, ${wordSize}), %rsp"
)

val end = List(
  "    movq %rcx, %rsp", // Restore original stack
  "    ret"
)

def makeLabel: C[Label] = for {
  lab <- EitherT.liftF(State.get)
  _ <- EitherT.liftF(State.set(lab + 1))
} yield s"L_$lab"

def constToImm(c: Const): String = c match
  case Const.Fixnum(n) => s"$$${n << fixShift}"
  case Const.False     => s"$$${falseVal}"
  case Const.True      => s"$$${trueVal}"
  case Const.Null      => s"$$${nullVal}"
  case Const.Ch(c)     => s"$$${(c << charShift) | charTag}"

def compileUnPrim(p: UnPrim): List[Instruction] = {
  val setBoolean = List(
    "    sete %al",
    "    movzbq %al, %rax",
    s"    salq $$${boolShift}, %rax",
    s"    orq $$${boolTag}, %rax"
  )
  p match
    case UnPrim.Inc =>
      List(s"    addq ${constToImm(Const.Fixnum(1))}, %rax")
    case UnPrim.Dec =>
      List(s"    subq ${constToImm(Const.Fixnum(1))}, %rax")
    case UnPrim.CharToFixnum =>
      List(s"    sarq $$${charShift - fixShift}, %rax")
    case UnPrim.FixnumToChar =>
      List(
        s"    salq $$${charShift - fixShift}, %rax",
        s"    orq $$${charTag}, %rax"
      )
    case UnPrim.IsZero =>
      "    cmp $0, %rax" :: setBoolean
    case UnPrim.IsNull =>
      s"    cmp $$${nullVal}, %rax" :: setBoolean
    case UnPrim.IsBool =>
      List(
        s"    and $$${boolMask}, %rax",
        s"    cmp $$${boolTag}, %rax"
      ) ++ setBoolean
    case UnPrim.IsFixnum =>
      List(
        s"    andq $$${fixMask}, %rax",
        s"    cmp $$${fixTag}, %rax"
      ) ++ setBoolean

    case _ => sys.error("Not implemented")
}

def compileBinPrim(stackIdx: Int, p: BinPrim): List[Instruction] = p match
  case BinPrim.Plus =>
    List(s"    addq ${stackIdx}(%rsp), %rax")

def compileBinOp(env: Env, stackIdx: Int, p: Exp.BinOp): C[List[Instruction]] =
  for {
    arg1 <- compileExp(env, stackIdx, p.e1)
    arg2 <- compileExp(env, stackIdx - wordSize, p.e2)
  } yield arg1 ++ List(
    s"    movq %rax, ${stackIdx}(%rsp)"
  ) ++ arg2 ++ compileBinPrim(stackIdx, p.primOp)

def compileIf(env: Env, stackIdx: Int, ifE: Exp.If): C[List[Instruction]] =
  for {
    altLabel <- makeLabel
    endLabel <- makeLabel
    testCode <- compileExp(env, stackIdx, ifE.test)
    thenCode <- compileExp(env, stackIdx, ifE.thenB)
    elseCode <- compileExp(env, stackIdx, ifE.elseB)
  } yield testCode ++ List(
    s"    cmp ${constToImm(Const.False)}, %al",
    s"    je $altLabel # jump to else"
  ) ++ thenCode ++ List(
    s"    jmp $endLabel",
    s"$altLabel: # else branch"
  ) ++ elseCode
    ++ List(s"$endLabel: # end of if")

def compileVar(env: Env, x: Name): Either[Error, List[Instruction]] =
  env.get(x) match
    case Some(Binding.StackPos(idx)) =>
      Right(List(s"    movq ${idx}(%rsp), %rax"))
    case _ => Left(s"Unknown variable '$x'")

def compileLet(
    env: Env,
    stackIdx: Int,
    bindings: List[(Name, Exp)],
    body: Exp
): C[List[Instruction]] = bindings match
  case Nil => compileExp(env, stackIdx, body)
  case (x, e) :: bs =>
    for {
      lCode <- compileExp(env, stackIdx, e)
      restCode <- compileLet(
        env + (x -> Binding.StackPos(stackIdx)),
        stackIdx - wordSize,
        bs,
        body
      )
    } yield lCode ++ List(s"    movq %rax, ${stackIdx}(%rsp)") ++ restCode

def compileApp(
    env: Env,
    stackIdx: Int,
    lvar: Name,
    args: List[Exp]
): C[List[Instruction]] = {

  def compileArgs(stackIdx: Int, args: List[Exp]): C[List[Instruction]] =
    args match
      case head :: next =>
        for {
          code <- compileExp(env, stackIdx, head)
          rest <- compileArgs(stackIdx - wordSize, next)
        } yield code ++ List(s"    movq %rax, ${stackIdx}(%rsp)") ++ rest
      case Nil => pure(List.empty)

  for {

    argsCode <- compileArgs(stackIdx - wordSize, args)
    lab <- env.get(lvar) match
      case Some(Binding.ProgramLabel(l)) => pure(l)
      case _                             => error("function $lvar not defined")

  } yield argsCode ++ List(
    s"    addq $$${stackIdx + wordSize}, %rsp", // Adjust stack pointer to be above local variables
    s"    call $lab # call ${lvar}",
    s"    addq $$${-(stackIdx + wordSize)}, %rsp" // Readjust
  )

}

final case class TopDef(label: Label, fd: FunDef)

def compileTopDef(
    env: Env,
    td: TopDef
): C[List[Instruction]] = {

  def go(env: Env, stackIdx: Int, formals: List[Name]): C[List[Instruction]] =
    formals match
      case f :: fs =>
        go(env + (f -> Binding.StackPos(stackIdx)), stackIdx - wordSize, fs)
      case Nil => compileExp(env, stackIdx, td.fd.body)

  go(env, -wordSize, td.fd.formals)
    .map(s"${td.label}: # fun ${td.fd.lvar}" :: _ ++ List("    ret"))
}

def compileProgram(p: Program): C[List[Instruction]] = {

  val topDefs: C[List[TopDef]] = p.funs
    .traverse(f => makeLabel.map(l => TopDef(l, f)))

  for {
    defs <- topDefs
    initEnv = defs.map(x => x.fd.lvar -> Binding.ProgramLabel(x.label)).toMap
    funs <- defs
      .traverse(td => compileTopDef(initEnv, td))
      .map(_.flatten)
    bodyCode <- compileExp(initEnv, -wordSize, p.body)
  } yield prelude ++ funs ++ entry ++ bodyCode ++ end

}

def compileExp(env: Env, stackIdx: Int, e: Exp): C[List[Instruction]] = e match
  case Exp.Var(x)     => EitherT.fromEither(compileVar(env, x))
  case Exp.CExp(c)    => EitherT.pure(List(s"    movq ${constToImm(c)}, %rax"))
  case ifE: Exp.If    => compileIf(env, stackIdx, ifE)
  case Exp.UnOp(p, e) => compileExp(env, stackIdx, e).map(_ ++ compileUnPrim(p))
  case binop: Exp.BinOp    => compileBinOp(env, stackIdx, binop)
  case Exp.Let(xs, body)   => compileLet(env, stackIdx, xs, body)
  case Exp.App(lvar, args) => compileApp(env, stackIdx, lvar, args)

def compile(p: Program): Either[Error, String] =
  compileProgram(p).value.runA(0).value.map(_.mkString("\n"))
