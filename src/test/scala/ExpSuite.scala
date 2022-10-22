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
      "'a'" -> "#\\a\n",
      "'0'" -> "#\\0\n"
    ).foreach(checkOutput.tupled)
  }

  test("boolean") {
    Seq(
      "true" -> "#t\n",
      "false" -> "#f\n",
      "!true" -> "#f\n",
      "!false" -> "#t\n",
      "!()" -> "#f\n",
      "!1234" -> "#f\n",
    ).foreach(checkOutput.tupled)
  }

  test("conv") {
    Seq(
      "int_to_char(65)" -> "#\\A\n",
      "char_to_int('a')" -> "97\n"
    ).foreach(checkOutput.tupled)
  }

  test("test") {
    Seq(
      "is_zero(65)" -> "#f\n",
      "is_zero(0)" -> "#t\n",

      "is_unit(0)" -> "#f\n",
      "is_unit(())" -> "#t\n",
      "is_unit(true)" -> "#f\n",

      "is_char('b')" -> "#t\n",
      "is_char(0)" -> "#f\n",
      "is_char(())" -> "#f\n",

      "is_int(1234)" -> "#t\n",
      "is_int(())" -> "#f\n",
      "is_int(0)" -> "#t\n",
      "is_int(false)" -> "#f\n",
      "is_int('b')" -> "#f\n"
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
      "1 == 3" -> "#f\n",
      "42 == 42" -> "#t\n",
      "true == true" -> "#t\n",
      "true == false" -> "#f\n",
      "1 == ()" -> "#f\n"
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
      Exp.Let(List(), Exp.CExp(Const.True)) -> "#t\n"
    ).foreach(checkOutput.tupled)
  }
}
