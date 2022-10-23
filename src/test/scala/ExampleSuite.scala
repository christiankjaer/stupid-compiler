import compiler.*
import interpreter.*
import parser.*

import java.io.File
import scala.io.Source

class ExampleSuite extends munit.FunSuite {

  def assertRight[A, B](x: Either[A, B], hint: String): B = x match
    case Left(value)  => fail(s"expected right, was $value, $hint")
    case Right(value) => value

  test("parse and compile examples") {

    val f = File("./examples")
    val examples = f.listFiles { (dir, name) => !name.endsWith(".no-compile") && !name.endsWith(".no-parse") }.toList

    examples.foreach { example =>
      val inputProgram = Source.fromFile(example).getLines().mkString
      val parsed = assertRight(parseProgram.parseAll(inputProgram), s"parsing ${example.getPath}")
      val compiled = assertRight(compile(parsed), s"compiling ${example.getPath}")
      val interpreted = assertRight(interpProgram(parsed), s"interpreting ${example.getPath}")
    }
  }

  test("should not parse") {

    val f = File("./examples")
    val examples = f.listFiles{ (dir, name) => name.endsWith(".no-parse") }.toList

    examples.foreach { example =>
      val inputProgram = Source.fromFile(example).getLines().mkString
      val parsed = assert(parseProgram.parseAll(inputProgram).isLeft, "${example.getPath} parsed but shouldn't")
    }
  }

  test("should not compile/interpret") {

    val f = File("./examples")
    val examples = f.listFiles{ (dir, name) => name.endsWith(".no-compile") }.toList

    examples.foreach { example =>
      val inputProgram = Source.fromFile(example).getLines().mkString
      val parsed = assertRight(parseProgram.parseAll(inputProgram), s"parsing ${example.getPath}")
      val compiled = assert(compile(parsed).isLeft, s"${example.getPath} compiled but shouldn't")
      val interpreted = assert(interpProgram(parsed).isLeft, s"interpreted ${example.getPath} but shouldn't")
    }
  }
}
