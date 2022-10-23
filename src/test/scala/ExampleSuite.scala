import java.io.File
import scala.io.Source

import parser.*
import compiler.*
import interpreter.*

class ExampleSuite extends munit.FunSuite {

  def assertRight[A, B](x: Either[A, B], hint: String): B = x match
    case Left(value)  => fail(s"expected right, was $value, $hint")
    case Right(value) => value

  test("parse and compile examples") {

    val f = File("./examples")
    val examples = f.listFiles.toList

    examples.foreach { example =>
      val inputProgram = Source.fromFile(example).getLines().mkString
      val parsed = assertRight(parseProgram.parseAll(inputProgram), s"parsing ${example.getPath}")
      val compiled = assertRight(compile(parsed), s"compiling ${example.getPath}")
      val interpreted = assertRight(interpProgram(parsed), s"interpreting ${example.getPath}")
    }
  }
}
