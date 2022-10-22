class ExpSuite extends CompilerSuite {


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
      Exp.UnOp(UnPrim.IsZero, Exp.CExp(Const.Fixnum(65))) -> "#f\n",
      Exp.UnOp(UnPrim.IsZero, Exp.CExp(Const.Fixnum(0))) -> "#t\n",
      Exp.UnOp(UnPrim.IsNull, Exp.CExp(Const.Fixnum(0))) -> "#f\n",
      Exp.UnOp(UnPrim.IsNull, Exp.CExp(Const.Null)) -> "#t\n",
      Exp.UnOp(UnPrim.IsBool, Exp.CExp(Const.Null)) -> "#f\n",
      Exp.UnOp(UnPrim.IsBool, Exp.CExp(Const.True)) -> "#t\n",
      Exp.UnOp(UnPrim.IsBool, Exp.CExp(Const.False)) -> "#t\n",
      Exp.UnOp(UnPrim.IsFixnum, Exp.CExp(Const.Fixnum(65))) -> "#t\n",
      Exp.UnOp(UnPrim.IsFixnum, Exp.CExp(Const.Fixnum(0))) -> "#t\n",
      Exp.UnOp(UnPrim.IsFixnum, Exp.CExp(Const.Null)) -> "#f\n",
      Exp.UnOp(UnPrim.IsFixnum, Exp.CExp(Const.Ch('b'))) -> "#f\n"
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

  test("add/sub") {

    Seq(
      Exp.BinOp(
        BinPrim.Plus,
        Exp.CExp(Const.Fixnum(3)),
        Exp.CExp(Const.Fixnum(4))
      ) -> "7\n",
      Exp.BinOp(
        BinPrim.Plus,
        Exp.BinOp(
          BinPrim.Plus,
          Exp.CExp(Const.Fixnum(3)),
          Exp.CExp(Const.Fixnum(4))
        ),
        Exp.CExp(Const.Fixnum(9))
      ) -> "16\n"
    ).foreach(checkOutput.tupled)

  }

  test("let") {
    Seq(
      Exp.Let(List(), Exp.CExp(Const.True)) -> "#t\n",
      Exp.Let(List("x" -> Exp.CExp(Const.Fixnum(10))), Exp.Var("x")) -> "10\n",
      Exp.Let(
        List(
          "x" -> Exp.CExp(Const.Fixnum(10)),
          "y" -> Exp.BinOp(BinPrim.Plus, Exp.Var("x"), Exp.Var("x"))
        ),
        Exp.Var("y")
      ) -> "20\n"
    ).foreach(checkOutput.tupled)
  }
}
