enum Const {
  case Fixnum(n: Long)
  case True
  case False
  case Null
}
enum Exp {
  case CExp(c: Const)
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
  case Const.Fixnum(n) => s"${n << 2}"
  case Const.True => "0x2F"
  case Const.False => "0x1F"
  case Const.Null => "0x3F"


def compileExp(e: Exp): Program = e match
  case Exp.CExp(c) => List(s"movq $$${constToImm(c)}, %rax")

def compile(e: Exp): String =
  Seq(prelude, compileExp(e).mkString("\n"), end).mkString("\n")

@main def hello: Unit =
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"
