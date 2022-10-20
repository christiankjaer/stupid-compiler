import sys.process.*
import java.nio.file.Files
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File

class MySuite extends munit.FunSuite {

  def getOutput(program: String): String = {

    val p = Files.createTempFile("program", ".s")
    val file = p.toFile()
    val bw = new FileWriter(file)
    bw.append(program)
    bw.close()

    val cmd = Process(
      Seq("zig", "build", "run", s"-Dentry=${p.toString}"),
      new File("./runtime")
    )
    cmd.!!
  }

  def checkOutput(program: Exp, expected: String): Unit = {
    assertEquals(getOutput(compileProgram(program)), expected)
  }

  test("fixnum") {
    Seq(
      Exp.CExp(Const.Fixnum(0)) -> "0\n",
      Exp.CExp(Const.Fixnum(1)) -> "1\n",
      Exp.CExp(Const.Fixnum(-1)) -> "-1\n",
      Exp.CExp(Const.Fixnum(10)) -> "10\n",
      Exp.CExp(Const.Fixnum(-10)) -> "-10\n",
      Exp.CExp(Const.Fixnum(2736)) -> "2736\n",
      Exp.CExp(Const.Fixnum(536870911)) -> "536870911\n",
      Exp.CExp(Const.Fixnum(-536870912)) -> "-536870912\n"
    ).foreach(checkOutput.tupled)
  }

  test("char") {
    Seq(
      Exp.CExp(Const.Ch('a')) -> "#\\a\n",
      Exp.CExp(Const.Ch('0')) -> "#\\0\n"
    ).foreach(checkOutput.tupled)

  }

  test("boolean") {

    Seq(
      Exp.CExp(Const.True) -> "#t\n",
      Exp.CExp(Const.False) -> "#f\n",
      Exp.CExp(Const.Null) -> "()\n"
    ).foreach(checkOutput.tupled)

  }

  test("inc") {
    Seq(
      Exp.UPrim(UnPrim.Inc, Exp.CExp(Const.Fixnum(10))) -> "11\n",
      Exp.UPrim(UnPrim.Inc, Exp.CExp(Const.Fixnum(0))) -> "1\n",
      Exp.UPrim(UnPrim.Inc, Exp.CExp(Const.Fixnum(-1))) -> "0\n"
    ).foreach(checkOutput.tupled)
  }

  test("dec") {
    Seq(
      Exp.UPrim(UnPrim.Dec, Exp.CExp(Const.Fixnum(10))) -> "9\n",
      Exp.UPrim(UnPrim.Dec, Exp.CExp(Const.Fixnum(1))) -> "0\n",
      Exp.UPrim(UnPrim.Dec, Exp.CExp(Const.Fixnum(0))) -> "-1\n"
    ).foreach(checkOutput.tupled)
  }

  test("if") {
    Seq(
      Exp.If(
        Exp.CExp(Const.True),
        Exp.CExp(Const.Fixnum(1337)),
        Exp.CExp(Const.Fixnum(42))
      ) -> "1337\n",
      Exp.If(
        Exp.CExp(Const.Null),
        Exp.CExp(Const.Fixnum(1337)),
        Exp.CExp(Const.Fixnum(42))
      ) -> "1337\n",
      Exp.If(
        Exp.CExp(Const.False),
        Exp.CExp(Const.Fixnum(1337)),
        Exp.CExp(Const.Fixnum(42))
      ) -> "42\n",
      Exp.If(
        Exp.If(
          Exp.CExp(Const.False),
          Exp.CExp(Const.False),
          Exp.CExp(Const.True)
        ), // True
        Exp.CExp(Const.Fixnum(1337)),
        Exp.CExp(Const.Fixnum(42))
      ) -> "1337\n"
    ).foreach(checkOutput.tupled)
  }

  test("add") {

    Seq(
      Exp.BPrim(
        BinPrim.Plus,
        Exp.CExp(Const.Fixnum(3)),
        Exp.CExp(Const.Fixnum(4))
      ) -> "7\n",
      Exp.BPrim(
        BinPrim.Plus,
        Exp.BPrim(
          BinPrim.Plus,
          Exp.CExp(Const.Fixnum(3)),
          Exp.CExp(Const.Fixnum(4))
        ),
        Exp.CExp(Const.Fixnum(9))
      ) -> "16\n"
    ).foreach(checkOutput.tupled)

  }
}
