import cats.syntax.all.*
import cats.data.{EitherT, State}

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

enum UnPrim {
  case Inc, Dec, CharToFixnum, FixnumToChar, IsZero, IsNull, Not, IsFixnum,
    IsBool, IsChar
}
enum BinPrim {
  case Plus
}
enum Const {
  case Fixnum(n: Long)
  case Ch(n: Char)
  case True, False, Null
}

type Name = String

enum Exp {
  case Var(x: Name)
  case CExp(c: Const)
  case UnOp(primOp: UnPrim, e: Exp)
  case BinOp(primOp: BinPrim, e1: Exp, e2: Exp)
  case If(test: Exp, thenB: Exp, elseB: Exp)
  case Let(bindings: List[(Name, Exp)], body: Exp)
}

type Instruction = String
type Program = List[Instruction]
type Label = String
type Env = Map[Name, Int]
type Error = String

type LabelCounter = Int

// Compile monad
type C[T] = EitherT[[x] =>> State[LabelCounter, x], Error, T]

val prelude =
  """    .text
    |    .globl    program_entry
    |    .type     program_entry, @function
    |program_entry:""".stripMargin('|')

// We move the stack (pointer is first argument (%rdi), size is second argument(%rsi)) into %rsp
val prepareStack = List(
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

def compileUnPrim(p: UnPrim): Program = {
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

def compileBinPrim(stackIdx: Int, p: BinPrim): Program = p match
  case BinPrim.Plus =>
    List(s"    addq ${stackIdx}(%rsp), %rax")

def compileBinOp(env: Env, stackIdx: Int, p: Exp.BinOp): C[Program] = for {
  arg1 <- compileExp(env, stackIdx, p.e1)
  arg2 <- compileExp(env, stackIdx - wordSize, p.e2)
} yield arg1 ++ List(
  s"    movq %rax, ${stackIdx}(%rsp)"
) ++ arg2 ++ compileBinPrim(stackIdx, p.primOp)

def compileIf(env: Env, stackIdx: Int, ifE: Exp.If): C[Program] = for {
  altLabel <- makeLabel
  endLabel <- makeLabel
  testCode <- compileExp(env, stackIdx, ifE.test)
  thenCode <- compileExp(env, stackIdx, ifE.thenB)
  elseCode <- compileExp(env, stackIdx, ifE.elseB)
} yield testCode ++ List(
  s"    cmp ${constToImm(Const.False)}, %al",
  s"    je $altLabel"
) ++ thenCode ++ List(
  s"    jmp $endLabel",
  s"$altLabel:"
) ++ elseCode
  ++ List(s"$endLabel:")

def compileVar(env: Env, x: Name): Either[Error, Program] = env.get(x) match
  case None      => Left(s"Unknown variable '$x'")
  case Some(idx) => Right(List(s"    movq ${idx}(%rsp), %rax"))

def compileLet(
    env: Env,
    stackIdx: Int,
    bindings: List[(Name, Exp)],
    body: Exp
): C[Program] = bindings match
  case Nil => compileExp(env, stackIdx, body)
  case (x, e) :: bs =>
    for {
      lCode <- compileExp(env, stackIdx, e)
      restCode <- compileLet(
        env + (x -> stackIdx),
        stackIdx - wordSize,
        bs,
        body
      )
    } yield lCode ++ List(s"    movq %rax, ${stackIdx}(%rsp)") ++ restCode

def compileExp(env: Env, stackIdx: Int, e: Exp): C[Program] = e match
  case Exp.Var(x)     => EitherT.fromEither(compileVar(env, x))
  case Exp.CExp(c)    => EitherT.pure(List(s"    movq ${constToImm(c)}, %rax"))
  case ifE: Exp.If    => compileIf(env, stackIdx, ifE)
  case Exp.UnOp(p, e) => compileExp(env, stackIdx, e).map(_ ++ compileUnPrim(p))
  case binop: Exp.BinOp  => compileBinOp(env, stackIdx, binop)
  case Exp.Let(xs, body) => compileLet(env, stackIdx, xs, body)

def compileProgram(e: Exp): Either[Error, String] = for {
  expCode <- compileExp(Map.empty, -wordSize, e).value
    .runA(0)
    .value
} yield List(
  prelude,
  prepareStack.mkString("\n"),
  expCode.mkString("\n"),
  end.mkString("\n")
)
  .mkString("\n")

@main def main: Unit =
  println("Main does not exist yet")
