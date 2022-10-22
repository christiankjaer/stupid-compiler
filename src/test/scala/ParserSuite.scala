import parser.*
import syntax.*
import Exp.*
class ParserSuite extends munit.FunSuite {

  def num(x: Int): Exp = CExp(Const.Int(x))

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
