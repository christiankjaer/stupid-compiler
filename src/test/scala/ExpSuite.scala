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
      "()" -> "()\n"
    ).foreach(checkOutput.tupled)
  }

  test("inc") {
    Seq(
      Exp.UnOp(UnPrim.Inc, Exp.CExp(Const.Fixnum(10))) -> "11\n",
      Exp.UnOp(
        UnPrim.Inc,
        Exp.UnOp(UnPrim.Inc, Exp.CExp(Const.Fixnum(10)))
      ) -> "12\n",
      Exp.UnOp(UnPrim.Inc, Exp.CExp(Const.Fixnum(0))) -> "1\n",
      Exp.UnOp(UnPrim.Inc, Exp.CExp(Const.Fixnum(-1))) -> "0\n"
    ).foreach(checkOutput.tupled)
  }

  test("dec") {
    Seq(
      Exp.UnOp(UnPrim.Dec, Exp.CExp(Const.Fixnum(10))) -> "9\n",
      Exp.UnOp(UnPrim.Dec, Exp.CExp(Const.Fixnum(1))) -> "0\n",
      Exp.UnOp(UnPrim.Dec, Exp.CExp(Const.Fixnum(0))) -> "-1\n"
    ).foreach(checkOutput.tupled)
  }

  test("conv") {
    Seq(
      Exp.UnOp(UnPrim.FixnumToChar, Exp.CExp(Const.Fixnum(65))) -> "#\\A\n",
      Exp.UnOp(UnPrim.CharToFixnum, Exp.CExp(Const.Ch('a'))) -> "97\n"
    ).foreach(checkOutput.tupled)
  }

  test("test") {
    Seq(
      "is_zero(65)" -> "#f\n",
      "is_unit(())" -> "#t\n"
    ).foreach(checkOutput.tupled)
    Seq(
      Exp.UnOp(UnPrim.IsZero, Exp.CExp(Const.Fixnum(65))) -> "#f\n",
      Exp.UnOp(UnPrim.IsZero, Exp.CExp(Const.Fixnum(0))) -> "#t\n",
      Exp.UnOp(UnPrim.IsUnit, Exp.CExp(Const.Fixnum(0))) -> "#f\n",
      Exp.UnOp(UnPrim.IsUnit, Exp.CExp(Const.Unit)) -> "#t\n",
      Exp.UnOp(UnPrim.IsBool, Exp.CExp(Const.Unit)) -> "#f\n",
      Exp.UnOp(UnPrim.IsBool, Exp.CExp(Const.True)) -> "#t\n",
      Exp.UnOp(UnPrim.IsBool, Exp.CExp(Const.False)) -> "#t\n",
      Exp.UnOp(UnPrim.IsFixnum, Exp.CExp(Const.Fixnum(65))) -> "#t\n",
      Exp.UnOp(UnPrim.IsFixnum, Exp.CExp(Const.Fixnum(0))) -> "#t\n",
      Exp.UnOp(UnPrim.IsFixnum, Exp.CExp(Const.Unit)) -> "#f\n",
      Exp.UnOp(UnPrim.IsFixnum, Exp.CExp(Const.Ch('b'))) -> "#f\n"
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

  test("add/sub") {

    Seq(
      "3 + 4" -> "7\n",
      "3 + 4 + 9" -> "16\n",
      "5 - 2" -> "3\n",
      "1 - 2" -> "-1\n"
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
