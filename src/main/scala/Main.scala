import cats.syntax.all.*
import cats.data.State

enum UnPrim {
  case Inc, Dec, CharToFixnum, FixNumToChar, IsZero, IsNull, Not, IsFixnum,
    IsBoolean, IsChar
}
enum BinPrim {
  case Plus, Minus
}
enum Const {
  case Fixnum(n: Long)
  case Ch(n: Char)
  case True, False, Null
}
enum Exp {
  case CExp(c: Const)
  case UPrim(primOp: UnPrim, e: Exp)
  case BPrim(primOp: BinPrim, e1: Exp, e2: Exp)
  case If(test: Exp, thenB: Exp, elseB: Exp)
}

type Instruction = String
type Program = List[Instruction]
type Label = String

type LabelCounter = Int

// Compile monad
type C[T] = State[LabelCounter, T]

val prelude =
  """    .text
    |    .globl    scheme_entry
    |    .type     scheme_entry, @function
    |scheme_entry:""".stripMargin('|')

val end = "ret"

def makeLabel: C[Label] = for {
  lab <- State.get
  _ <- State.set(lab + 1)
} yield s"L_$lab"

def constToImm(c: Const): String = c match
  case Const.Fixnum(n) => s"$$${n << 2}"
  case Const.True      => "$0x2F"
  case Const.False     => "$0x1F"
  case Const.Null      => "$0x3F"
  case Const.Ch(c)     => s"$$0x${c.toInt.toHexString}0F"

def compileUPrim(p: UnPrim): Program = p match
  case UnPrim.Inc =>
    List(s"addq ${constToImm(Const.Fixnum(1))}, %rax")
  case UnPrim.Dec =>
    List(s"subq ${constToImm(Const.Fixnum(1))}, %rax")
  case _ => ???

def compileExp(e: Exp): C[Program] = e match
  case Exp.CExp(c) => State.pure(List(s"movq ${constToImm(c)}, %rax"))
  case Exp.If(test, thenB, elseB) =>
    for {
      altLabel <- makeLabel
      endLabel <- makeLabel
      testCode <- compileExp(test)
      thenCode <- compileExp(thenB)
      elseCode <- compileExp(elseB)
    } yield testCode ++ List(
      "cmp $0x1F, %al",
      s"je $altLabel"
    ) ++ thenCode ++ List(
      s"jmp $endLabel",
      s"$altLabel:"
    ) ++ elseCode
      ++ List(s"$endLabel:")

  case Exp.UPrim(p, e) =>
    for {
      arg <- compileExp(e)
    } yield arg ++ compileUPrim(p)

def compile(e: Exp): String =
  Seq(prelude, compileExp(e).runA(0).value.mkString("\n"), end).mkString("\n")

@main def hello: Unit =
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"
