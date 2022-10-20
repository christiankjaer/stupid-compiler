enum UnPrim {
  case Inc, Dec, CharToFixnum, FixNumToChar, IsZero, IsNull, Not, IsFixnum,
    IsBoolean, IsChar
}
enum Const {
  case Fixnum(n: Long)
  case Ch(n: Char)
  case True, False, Null
}
enum Exp {
  case CExp(c: Const)
  case UPrim(primOp: UnPrim, e: Exp)
}

type Instruction = String
type Program = List[Instruction]

val prelude =
  """    .text
    |    .globl    scheme_entry
    |    .type     scheme_entry, @function
    |scheme_entry:""".stripMargin('|')

val end = "ret"

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

def compileExp(e: Exp): Program = e match
  case Exp.CExp(c)     => List(s"movq ${constToImm(c)}, %rax")
  case Exp.UPrim(p, e) => compileExp(e) ++ compileUPrim(p)

def compile(e: Exp): String =
  Seq(prelude, compileExp(e).mkString("\n"), end).mkString("\n")

@main def hello: Unit =
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"
