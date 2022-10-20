import cats.syntax.all.*
import cats.data.{EitherT, State}

val wordSize = 8

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
    |    .globl    scheme_entry
    |    .type     scheme_entry, @function
    |scheme_entry:""".stripMargin('|')

// We move the stack (pointer is first argument (%rdi), size is second argument(%rsi)) into %rbx
val prepareStack =
  s"    leaq (%rdi, %rsi, ${wordSize}), %rbx"

val end = "    ret"

def makeLabel: C[Label] = for {
  lab <- EitherT.liftF(State.get)
  _ <- EitherT.liftF(State.set(lab + 1))
} yield s"L_$lab"

def constToImm(c: Const): String = c match
  case Const.Fixnum(n) => s"$$${n << 2}"
  case Const.False     => "$0x1F"
  case Const.True      => "$0x2F"
  case Const.Null      => "$0x3F"
  case Const.Ch(c)     => s"$$0x${c.toInt.toHexString}0F"

def compileUnPrim(p: UnPrim): Program = p match
  case UnPrim.Inc =>
    List(s"    addq ${constToImm(Const.Fixnum(1))}, %rax")
  case UnPrim.Dec =>
    List(s"    subq ${constToImm(Const.Fixnum(1))}, %rax")
  case _ => sys.error("Not implemented")

def compileBinPrim(stackIdx: Int, p: BinPrim): Program = p match
  case BinPrim.Plus =>
    List(s"    addq ${stackIdx}(%rbx), %rax")

def compileBinOp(env: Env, stackIdx: Int, p: Exp.BinOp): C[Program] = for {
  arg1 <- compileExp(env, stackIdx, p.e1)
  arg2 <- compileExp(env, stackIdx - wordSize, p.e2)
} yield arg1 ++ List(
  s"    movq %rax, ${stackIdx}(%rbx)"
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
  case Some(idx) => Right(List(s"    movq ${idx}(%rbx), %rax"))

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
    } yield lCode ++ List(s"    movq %rax, ${stackIdx}(%rbx)") ++ restCode

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
  prepareStack,
  expCode.mkString("\n"),
  end
)
  .mkString("\n")

@main def main: Unit =
  println("Main does not exist yet")
