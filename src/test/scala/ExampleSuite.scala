import compiler.*
import interpreter.*
import parser.*
import syntax.Const

import java.io.File
import scala.io.Source

class ExampleSuite extends compiler.CompilerSuite {

  def constToString(c: Const): String = c match
    case Const.Int(n)  => n.toString
    case Const.Ch(n)   => c.toString
    case Const.Bool(b) => b.toString
    case Const.Unit    => "()"

  def assertRight[A, B](x: Either[A, B], hint: String): B = x match
    case Left(value)  => fail(s"expected right, was $value, $hint")
    case Right(value) => value

  val f = File("./examples")

  val positiveExamples = f.listFiles { (dir, name) =>
    !name.endsWith(".no-compile") && !name.endsWith(".no-parse")
  }.toList

  val expectedRegex = "# output: (.*)".r

  positiveExamples.foreach { example =>
    test(example.getPath) {

      val inputProgram = Source.fromFile(example).getLines().mkString("\n")
      val expectedRegex(output) = inputProgram.linesIterator.toList.head: @unchecked
      val parsed = assertRight(parseProgram.parseAll(inputProgram), s"parsing ${example.getPath}")

      checkOutput(inputProgram, s"$output\n")
      assertEquals(interpProgram(parsed).map(constToString), Right(output))
    }
  }

  val noParse = f.listFiles { (dir, name) => name.endsWith(".no-parse") }.toList

  noParse.foreach { example =>
    test(example.getPath) {
      val inputProgram = Source.fromFile(example).getLines().mkString("\n")
      val parsed =
        assert(parseProgram.parseAll(inputProgram).isLeft, s"${example.getPath} parsed but shouldn't")
    }
  }

  val noCompile = f.listFiles { (dir, name) => name.endsWith(".no-compile") }.toList

  noCompile.foreach { example =>
    test(example.getPath) {
      val inputProgram = Source.fromFile(example).getLines().mkString("\n")
      val parsed = assertRight(parseProgram.parseAll(inputProgram), s"parsing ${example.getPath}")
      val compiled = assert(compile(parsed).isLeft, s"${example.getPath} compiled but shouldn't")
      val interpreted = assert(interpProgram(parsed).isLeft, s"interpreted ${example.getPath} but shouldn't")
    }
  }
}
