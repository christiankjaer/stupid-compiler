@main def main: Unit = {

  val program = Program(
    List(
      FunDef(
        "f",
        Lambda(
          List("x", "y"),
          Exp.BinOp(BinPrim.Plus, Exp.Var("y"), Exp.Var("x"))
        )
      )
    ),
    Exp.App("f", List(Exp.CExp(Const.Fixnum(3)), Exp.CExp(Const.Fixnum(5))))
  )

  println(compile(program).getOrElse("error"))

}
