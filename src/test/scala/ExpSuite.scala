import syntax.*
class ExpSuite extends CompilerSuite {

  test("fixnum") {
    Seq(
      "0" -> "0\n",
      "1" -> "1\n",
      "-1" -> "-1\n",
      "10" -> "10\n",
      "-10" -> "-10\n",
      "2736" -> "2736\n",
      "536870911" -> "536870911\n",
      "-536870912" -> "-536870912\n"
    ).foreach(checkOutput.tupled)
  }

  test("char") {
    Seq(
      "'a'" -> "a\n",
      "'0'" -> "0\n"
    ).foreach(checkOutput.tupled)
  }

  test("boolean") {
    Seq(
      "true" -> "true\n",
      "false" -> "false\n",
      "!true" -> "false\n",
      "!false" -> "true\n",
      "!()" -> "false\n",
      "!1234" -> "false\n",
    ).foreach(checkOutput.tupled)
  }

  test("conv") {
    Seq(
      "int_to_char(65)" -> "A\n",
      "char_to_int('a')" -> "97\n"
    ).foreach(checkOutput.tupled)
  }

  test("test") {
    Seq(
      "is_zero(65)" -> "false\n",
      "is_zero(0)" -> "true\n",

      "is_unit(0)" -> "false\n",
      "is_unit(())" -> "true\n",
      "is_unit(true)" -> "false\n",

      "is_char('b')" -> "true\n",
      "is_char(0)" -> "false\n",
      "is_char(())" -> "false\n",

      "is_int(1234)" -> "true\n",
      "is_int(())" -> "false\n",
      "is_int(0)" -> "true\n",
      "is_int(false)" -> "false\n",
      "is_int('b')" -> "false\n"
    ).foreach(checkOutput.tupled)
  }

  test("if") {
    Seq(
      "if true then 1337 else 42" -> "1337\n",
      "if () then 1337 else 42" -> "1337\n",
      "if false then 1337 else 42" -> "42\n",
      "if false then 1 else if true then 2 else 3" -> "2\n",
      "if (if false then false else true) then 1337 else 42" -> "1337\n"
    ).foreach(checkOutput.tupled)
  }

  test("add/sub/mul/div") {

    Seq(
      "~3" -> "-3\n",
      "~(-3)" -> "3\n",
      "~0" -> "0\n",
      "~1337" -> "-1337\n",
      "3 + 4" -> "7\n",
      "3 + 4 + 9" -> "16\n",
      "5 - 2" -> "3\n",
      "1 - 2" -> "-1\n",
      "3 * 5" -> "15\n",
      "15 / 3" -> "5\n",
      "10 / 3" -> "3\n",
    ).foreach(checkOutput.tupled)

  }

  test("cmp") {
    Seq(
      "1 == 3" -> "false\n",
      "42 == 42" -> "true\n",
      "true == true" -> "true\n",
      "true == false" -> "false\n",
      "1 == ()" -> "false\n"
    ).foreach(checkOutput.tupled)

  }

  test("let") {

    Seq(
      "let x = 10 in x" -> "10\n",
      """|let x = 10
         |    y = x + x
         | in y""".stripMargin('|') -> "20\n",
      """|let x = if false then 10 else 12
         |    y = x + x
         | in y""".stripMargin('|') -> "24\n"
    ).foreach(checkOutput.tupled)

    // Test for empty let
    Seq(
      Exp.Let(List(), Exp.CExp(Const.True)) -> "true\n"
    ).foreach(checkOutput.tupled)
  }
}
