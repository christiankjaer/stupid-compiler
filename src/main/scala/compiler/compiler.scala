package compiler

import cats.data.{EitherT, State}
import cats.syntax.all.*
import syntax.*

val wordSize = 8
val charShift = 8
val charTag = 0x0f
val charMask = 0xff

val intShift = 2
val intMask = 0x3
val intTag = 0x0

val falseVal = 0x2f
val trueVal = 0x6f
val unitVal = 0x3f

val boolShift = 6
val boolMask = 0xbf
val boolTag = 0x2f

enum Binding {
  case StackPos(i: Int)
  case ProgramLabel(l: Label)
  case BIn(b: Builtin)
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

val baseEnv: Map[Name, Binding] =
  builtins.map((k, v) => k -> Binding.BIn(v))

def makeLabel: C[Label] = for {
  lab <- EitherT.liftF(State.get)
  _ <- EitherT.liftF(State.set(lab + 1))
} yield s"L_$lab"

def constToImm(c: Const): String = c match
  case Const.Int(n)      => s"$$${n << intShift}"
  case Const.Bool(false) => s"$$${falseVal}"
  case Const.Bool(true)  => s"$$${trueVal}"
  case Const.Unit        => s"$$${unitVal}"
  case Const.Ch(c)       => s"$$${(c << charShift) | charTag}"

val setBoolean = List(
  "    sete %al",
  "    movzbq %al, %rax",
  s"    salq $$${boolShift}, %rax",
  s"    orq $$${boolTag}, %rax"
)

def compileUnPrim(p: UnPrim): List[Instruction] =
  p match
    case UnPrim.Neg =>
      List(
        "    negq %rax",
        "    and $0xFC, %al"
      )
    case UnPrim.CharToInt =>
      List(s"    sarq $$${charShift - intShift}, %rax")
    case UnPrim.IntToChar =>
      List(
        s"    salq $$${charShift - intShift}, %rax",
        s"    orq $$${charTag}, %rax"
      )
    case UnPrim.IsZero =>
      "    cmp $0, %rax" :: setBoolean
    case UnPrim.IsUnit =>
      s"    cmp $$${unitVal}, %rax" :: setBoolean
    case UnPrim.IsBool =>
      List(
        s"    and $$${boolMask}, %rax",
        s"    cmp $$${boolTag}, %rax"
      ) ++ setBoolean
    case UnPrim.IsInt =>
      List(
        s"    andq $$${intMask}, %rax",
        s"    cmp $$${intTag}, %rax"
      ) ++ setBoolean
    case UnPrim.IsChar =>
      List(
        s"    andq $$${charMask}, %rax",
        s"    cmp $$${charTag}, %rax"
      ) ++ setBoolean
    case UnPrim.Not =>
      s"    cmpq $$${falseVal}, %rax" :: setBoolean

def compileBinPrim(stackIdx: Int, p: BinPrim): List[Instruction] = p match
  case BinPrim.Plus =>
    List(s"    addq ${stackIdx}(%rsp), %rax")
  case BinPrim.Minus =>
    List(
      s"    xchgq ${stackIdx}(%rsp), %rax", // There is definitely a better way
      s"    subq ${stackIdx}(%rsp), %rax"
    )
  case BinPrim.Times =>
    List(
      s"    sarq $$${intShift}, %rax",
      s"    imulq ${stackIdx}(%rsp), %rax"
    )
  case BinPrim.Div =>
    List(
      s"    xchgq ${stackIdx}(%rsp), %rax", // There is definitely a better way
      "    cqto",
      s"    idivq ${stackIdx}(%rsp), %rax",
      s"    salq $$${intShift}, %rax"
    )
  case BinPrim.Eq =>
    s"    cmpq ${stackIdx}(%rsp), %rax" :: setBoolean

def compileBinOp(env: Env, stackIdx: Int, bin: Exp.BinOp): C[List[Instruction]] =
  for {
    arg1 <- compileExp(env, stackIdx, bin.e1)
    arg2 <- compileExp(env, stackIdx - wordSize, bin.e2)
  } yield arg1 ++ List(
    s"    movq %rax, ${stackIdx}(%rsp)"
  ) ++ arg2 ++ compileBinPrim(stackIdx, bin.prim)

def compileIf(env: Env, stackIdx: Int, ifE: Exp.If): C[List[Instruction]] =
  for {
    altLabel <- makeLabel
    endLabel <- makeLabel
    testCode <- compileExp(env, stackIdx, ifE.test)
    thenCode <- compileExp(env, stackIdx, ifE.thenB)
    elseCode <- compileExp(env, stackIdx, ifE.elseB)
  } yield testCode ++ List(
    s"    cmp ${constToImm(Const.Bool(false))}, %al",
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
    label: Label,
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

  } yield argsCode ++ List(
    s"    addq $$${stackIdx + wordSize}, %rsp", // Adjust stack pointer to be above local variables
    s"    call $label",
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
    .map(s"${td.label}: # fun ${td.fd.name}" :: _ ++ List("    ret"))
}

def compileProgram(p: Program): C[List[Instruction]] = {

  val topDefs: C[List[TopDef]] = p.funs
    .traverse(f => makeLabel.map(l => TopDef(l, f)))

  for {
    defs <- topDefs
    initEnv = baseEnv ++ defs
      .map(x => x.fd.name -> Binding.ProgramLabel(x.label))
      .toMap
    funs <- defs.flatTraverse(td => compileTopDef(initEnv, td))
    bodyCode <- compileExp(initEnv, -wordSize, p.body)
  } yield prelude ++ funs ++ entry ++ bodyCode ++ end
}

def compileExp(env: Env, stackIdx: Int, e: Exp): C[List[Instruction]] = e match
  case Exp.Var(x)        => EitherT.fromEither(compileVar(env, x))
  case Exp.CExp(c)       => EitherT.pure(List(s"    movq ${constToImm(c)}, %rax"))
  case ifE: Exp.If       => compileIf(env, stackIdx, ifE)
  case Exp.UnOp(p, e)    => compileExp(env, stackIdx, e).map(_ ++ compileUnPrim(p))
  case binop: Exp.BinOp  => compileBinOp(env, stackIdx, binop)
  case Exp.Let(xs, body) => compileLet(env, stackIdx, xs, body)
  case Exp.App(lvar, args) =>
    (env.get(lvar), args) match {
      case (Some(Binding.ProgramLabel(l)), _) =>
        compileApp(env, stackIdx, l, args)
      case (Some(Binding.BIn(Builtin.Unary(f))), List(e)) =>
        compileExp(env, stackIdx, f(e))
      case (Some(Binding.BIn(Builtin.Binary(f))), List(e1, e2)) =>
        compileExp(env, stackIdx, f(e1, e2))
      case (_, _) => error("Unbound function")
    }

def compile(p: Program): Either[Error, String] =
  compileProgram(p).value.runA(0).value.map(_.mkString("\n"))
