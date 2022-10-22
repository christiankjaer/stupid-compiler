import syntax.*
import compiler.*

@main def main: Unit = {
  println(
    parser.parseProgram
      .parseAll("fun add(x, y) = x + y in add(10, 20)")
      .flatMap(compile)
      .getOrElse("")
  )

}
