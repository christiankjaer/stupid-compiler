import syntax.*

class LetRecSuite extends CompilerSuite {

  test("not used") {
    checkOutput("fun f() = 1337 in ()", "()\n")
  }

  test("no args") {
    checkOutput("fun f() = 1337 in f()", "1337\n")
  }

  test("scoping") {
    checkOutput(
      """|fun f() = 1337
         |fun g() = f()
         |in g()""".stripMargin('|'),
      "1337\n"
    )
  }

  test("add") {
    val p =
      """|fun g(x) = x
         |fun f(x, y) = g(y) + x
         |in let x = 3
         |in f(x, 5)""".stripMargin
    checkOutput(p, "8\n")
  }

  test("recursion") {
    val p =
      """|fun add(x, y) = if is_zero(y) then x else add(x+1, y-1)
         |in add(100, 400)
         |""".stripMargin
    checkOutput(p, "500\n")

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
