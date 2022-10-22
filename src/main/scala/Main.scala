import syntax.*
import compiler.*

@main def main: Unit = {
  println(
    parser.parseProgram
      .parseAll(
        """|fun id(x) = x
           |fun add(x, y) = x + y
           |in id(add(10, 20))""".stripMargin('|'))
      .flatMap(compile)
      .getOrElse("")
  )

}
