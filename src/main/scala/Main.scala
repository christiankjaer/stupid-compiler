import cats.syntax.all.*
import cats.data.State

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

val prepareStack =
  s"    leaq (%rdi, %rsi, ${wordSize}), %rbx"

val end = "    ret"

def makeLabel: C[Label] = for {
  lab <- State.get
  _ <- State.set(lab + 1)
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

def compileBPrim(stackIdx: Int, p: Exp.BPrim): C[Program] = for {
  arg1 <- compileExp(stackIdx, p.e1)
  arg2 <- compileExp(stackIdx - wordSize, p.e2)
} yield arg1 ++ List(
  s"    movq %rax, ${stackIdx}(%rbx)"
) ++ arg2 ++ compileBinPrim(stackIdx, p.primOp)

def compileIf(stackIdx: Int, ifE: Exp.If): C[Program] = for {
  altLabel <- makeLabel
  endLabel <- makeLabel
  testCode <- compileExp(stackIdx, ifE.test)
  thenCode <- compileExp(stackIdx, ifE.thenB)
  elseCode <- compileExp(stackIdx, ifE.elseB)
} yield testCode ++ List(
  s"    cmp ${constToImm(Const.False)}, %al",
  s"    je $altLabel"
) ++ thenCode ++ List(
  s"    jmp $endLabel",
  s"$altLabel:"
) ++ elseCode
  ++ List(s"$endLabel:")

def compileExp(stackIdx: Int, e: Exp): C[Program] = e match
  case Exp.CExp(c)     => State.pure(List(s"    movq ${constToImm(c)}, %rax"))
  case ifE: Exp.If     => compileIf(stackIdx, ifE)
  case Exp.UPrim(p, e) => compileExp(stackIdx, e).map(_ ++ compileUnPrim(p))
  case bE: Exp.BPrim   => compileBPrim(stackIdx, bE)

def compileProgram(e: Exp): String =
  Seq(
    prelude,
    prepareStack,
    compileExp(-wordSize, e).runA(0).value.mkString("\n"),
    end
  )
    .mkString("\n")

@main def main: Unit =
  println("Main does not exist yet")
