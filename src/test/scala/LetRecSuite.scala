import syntax.*

class LetRecSuite extends CompilerSuite {

  test("not used") {
    val program = Program(
      List(
        FunDef("f", List.empty, Exp.CExp(Const.Fixnum(1337)))
      ),
      Exp.CExp(Const.Null)
    )
    checkOutput(program, "()\n")
  }

  test("add") {
    val program = Program(
      List(
        FunDef(
          "g",
          List("x"),
          Exp.Var("x")
        ),
        FunDef(
          "f",
          List("x", "y"),
          Exp.BinOp(
            BinPrim.Plus,
            Exp.App("g", List(Exp.Var("y"))),
            Exp.Var("x")
          )
        )
      ),
      Exp.Let(
        List("x" -> Exp.CExp(Const.Fixnum(3))),
        Exp.App("f", List(Exp.Var("x"), Exp.CExp(Const.Fixnum(5))))
      )
    )
    checkOutput(program, "8\n")
  }

  test("recursion") {
    val program = Program(
      List(
        FunDef(
          "add",
          List("x", "y"),
          Exp.If(
            Exp.UnOp(UnPrim.IsZero, Exp.Var("y")),
            Exp.Var("x"),
            Exp.App(
              "add",
              List(
                Exp.UnOp(UnPrim.Inc, Exp.Var("x")),
                Exp.UnOp(UnPrim.Dec, Exp.Var("y"))
              )
            )
          )
        )
      ),
      Exp.App("add", List(Exp.CExp(Const.Fixnum(3)), Exp.CExp(Const.Fixnum(7))))
    )
    checkOutput(program, "10\n")

  }
  test("addfun") {
    checkOutput(
      """|fun id(x) = x
         |fun add(x, y) = x + y
         |in id(add(3, 7))""".stripMargin('|'),
      "10\n"
    )
  }

}
