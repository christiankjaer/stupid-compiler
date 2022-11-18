package parser

import syntax.*

class ParserSuite extends munit.FunSuite {

  import Exp.*

  def num(x: Int): Exp[Unit] = C(Const.Int(x), ())
  def n(x: Int): Program[Unit] = Program(List.empty, num(x))

  // Ignore annotations for tests
  def parseE(s: String) = parseExp.parseAll(s).map(_.map(_ => ()))
  def parseP(s: String) = parseProgram.parseAll(s).map(_.map(_ => ()))

  test("comment") {

    assert(
      parseP("""|# comment
                |fun foo(x) = x + 3
                |in foo(1234)""".stripMargin).isRight
    )

    assertEquals(
      parseP("""|# comment
                |fun foo() = 10
                |in foo() # comment""".stripMargin),
      parseP("fun foo() = 10 in foo()")
    )

  }

  test("if") {
    assertEquals(
      parseE("if true then 1 else 2"),
      Right(If(C(Const.Bool(true), ()), C(Const.Int(1), ()), C(Const.Int(2), ()), ()))
    )
    assertEquals(
      parseE("if true then if false then 1 else 2 else 3"),
      Right(
        If(
          C(Const.Bool(true), ()),
          If(C(Const.Bool(false), ()), C(Const.Int(1), ()), C(Const.Int(2), ()), ()),
          C(Const.Int(3), ()),
          ()
        )
      )
    )
    assertEquals(
      parseE("if true then 1 else if false then 2 else 3"),
      Right(
        If(
          C(Const.Bool(true), ()),
          C(Const.Int(1), ()),
          If(C(Const.Bool(false), ()), C(Const.Int(2), ()), C(Const.Int(3), ()), ()),
          ()
        )
      )
    )
  }

  test("arithmetic expressions") {

    assertEquals(
      parseE("1 + (2 + 3)"),
      Right(BinOp(BinPrim.Plus, num(1), BinOp(BinPrim.Plus, num(2), num(3), ()), ()))
    )
    assertEquals(
      parseE("f(f(x))"),
      Right(App("f", List(App("f", List(Var("x", ())), ())), ()))
    )
    assertEquals(
      parseE("f + 1"),
      Right(BinOp(BinPrim.Plus, Var("f", ()), num(1), ()))
    )
    assertEquals(
      parseE("1 + f(1)"),
      Right(BinOp(BinPrim.Plus, num(1), App("f", List(num(1)), ()), ()))
    )
    assertEquals(
      parseE("f(1) + 1"),
      Right(BinOp(BinPrim.Plus, App("f", List(num(1)), ()), num(1), ()))
    )
    assertEquals(
      parseE("1 + 9"),
      Right(BinOp(BinPrim.Plus, num(1), num(9), ()))
    )
    assertEquals(parseE("1"), Right(num(1)))
    assertEquals(parseE("(1)"), Right(num(1)))
    assertEquals(parseE("f(1)"), Right(App("f", List(num(1)), ())))

  }

}
