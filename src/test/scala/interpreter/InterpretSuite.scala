package interpreter

import parser.*
import syntax.*

class InterpretSuite extends munit.FunSuite {

  def int(i: Long): Const = Const.Int(i)
  def vTrue: Const = Const.Bool(true)
  def vFalse: Const = Const.Bool(false)

  def check(p: Exp[SourceLocation], expected: Const): Unit = {
    assertEquals(
      interpProgram(Program(List.empty, p)),
      Right(expected)
    )
  }

  def check(p: String, expected: Const): Unit = {
    assertEquals(
      parseProgram.parseAll(p).flatMap(interpProgram),
      Right(expected)
    )
  }

  test("examples") {

    Seq(
      "123" -> int(123),
      "true" -> vTrue,
      "false" -> vFalse,
      "()" -> Const.Unit,
      "let x = 10 in x" -> int(10),
      "fun f(x) = x in f(10)" -> int(10),
      "fun f(x, y) = y in f(10, 20)" -> int(20),
      "fun f(x) = x fun g(x) = f(10) + x in g(5)" -> int(15)
    ).foreach(check.tupled)

  }

  test("not used") {
    check("fun f() = 1337 in ()", Const.Unit)
  }

  test("no args") {
    check("fun f() = 1337 in f()", int(1337))
  }

  test("scoping") {
    check(
      """|fun f() = 1337
         |fun g() = f()
         |in g()""".stripMargin('|'),
      int(1337)
    )
  }

  test("add") {
    val p =
      """|fun g(x) = x
         |fun f(x, y) = g(y) + x
         |in let x = 3
         |in f(x, 5)""".stripMargin
    check(p, int(8))
  }

  test("recursion") {
    val p =
      """|fun add(x, y) = if is_zero(y) then x else add(x+1, y-1)
         |in add(100, 400)
         |""".stripMargin
    check(p, int(500))
  }

  test("addfun") {
    check(
      """|fun id(x) = x
         |fun add(x, y) = x + y
         |in id(add(3, 7))""".stripMargin('|'),
      int(10)
    )
  }

  test("test") {
    Seq(
      "is_zero(65)" -> vFalse,
      "is_zero(0)" -> vTrue,
      "is_unit(0)" -> vFalse,
      "is_unit(())" -> vTrue,
      "is_unit(true)" -> vFalse,
      "is_char('b')" -> vTrue,
      "is_char(0)" -> vFalse,
      "is_char(())" -> vFalse,
      "is_int(1234)" -> vTrue,
      "is_int(())" -> vFalse,
      "is_int(0)" -> vTrue,
      "is_int(false)" -> vFalse,
      "is_int('b')" -> vFalse
    ).foreach(check.tupled)
  }

  test("let") {

    Seq(
      "let x = 10 in x" -> int(10),
      """|let x = 10
         |    y = x + x
         | in y""".stripMargin('|') -> int(20),
      """|let x = if false then 10 else 12
         |    y = x + x
         | in y""".stripMargin('|') -> int(24)
    ).foreach(check.tupled)

    check(Exp.Let(List(), Exp.C(vTrue, SourceLocation.start), SourceLocation.start), vTrue)
  }
  test("if") {
    Seq(
      "if true then 1337 else 42" -> int(1337),
      "if () then 1337 else 42" -> int(1337),
      "if false then 1337 else 42" -> int(42),
      "if false then 1 else if true then 2 else 3" -> int(2),
      "if (if false then false else true) then 1337 else 42" -> int(1337)
    ).foreach(check.tupled)
  }

  test("add/sub/mul/div") {

    Seq(
      "~3" -> int(-3),
      "~(-3)" -> int(3),
      "~0" -> int(0),
      "~1337" -> int(-1337),
      "3 + 4" -> int(7),
      "3 + 4 + 9" -> int(16),
      "5 - 2" -> int(3),
      "1 - 2" -> int(-1),
      "3 * 5" -> int(15),
      "15 / 3" -> int(5),
      "10 / 3" -> int(3)
    ).foreach(check.tupled)

  }

  test("conv") {
    Seq(
      "int_to_char(65)" -> Const.Ch('A'),
      "char_to_int('a')" -> int(97)
    ).foreach(check.tupled)
  }

  test("cmp") {
    Seq(
      "1 == 3" -> vFalse,
      "42 == 42" -> vTrue,
      "true == true" -> vTrue,
      "true == false" -> vFalse,
      "1 == ()" -> vFalse
    ).foreach(check.tupled)
  }

}
