package parser

import syntax.*

class ParserSuite extends munit.FunSuite {

  import Exp.*

  def num(x: Int): Exp = C(Const.Int(x))
  def n(x: Int): Program = Program(List.empty, num(x))

  test("comment") {

    assert(
      parseProgram
        .parseAll("""|# comment
                     |fun foo(x) = x + 3
                     |in foo(1234)""".stripMargin)
        .isRight
    )

    assertEquals(
      parseProgram.parseAll("""|# comment
           |fun foo() = 10
           |in foo() # comment""".stripMargin),
      parseProgram.parseAll("fun foo() = 10 in foo()")
    )

  }

  test("fucking plus") {

    assertEquals(
      parseExp.parseAll("1 + (2 + 3)"),
      Right(BinOp(BinPrim.Plus, num(1), BinOp(BinPrim.Plus, num(2), num(3))))
    )
    assertEquals(
      parseExp.parseAll("f(f(x))"),
      Right(App("f", List(App("f", List(Var("x"))))))
    )
    assertEquals(
      parseExp.parseAll("f + 1"),
      Right(BinOp(BinPrim.Plus, Var("f"), num(1)))
    )
    assertEquals(
      parseExp.parseAll("1 + f(1)"),
      Right(BinOp(BinPrim.Plus, num(1), App("f", List(num(1)))))
    )
    assertEquals(
      parseExp.parseAll("f(1) + 1"),
      Right(BinOp(BinPrim.Plus, App("f", List(num(1))), num(1)))
    )
    assertEquals(
      parseExp.parseAll("1 + 9"),
      Right(BinOp(BinPrim.Plus, num(1), num(9)))
    )
    assertEquals(parseExp.parseAll("1"), Right(num(1)))
    assertEquals(parseExp.parseAll("(1)"), Right(num(1)))
    assertEquals(parseExp.parseAll("f(1)"), Right(App("f", List(num(1)))))

  }

}
