package compiler

import compiler.*
import syntax.*

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.Files

import sys.process.*

abstract class CompilerSuite extends munit.FunSuite {
  def getOutput(program: String): String = {

    val p = Files.createTempFile("program", ".s")
    val file = p.toFile()
    val bw = new FileWriter(file)
    bw.append(program)
    bw.close()

    val cmd = Process(
      Seq("zig", "build", "run", s"-Dentry=${p.toString}"),
      new File("./runtime")
    )
    cmd.!!
  }

  def checkOutput(e: Exp, expected: String): Unit = {
    assertEquals(
      compile(Program(List.empty, e)).map(getOutput),
      Right(expected)
    )
  }

  def checkOutput(p: Program, expected: String): Unit = {
    assertEquals(
      compile(p).map(getOutput),
      Right(expected)
    )
  }

  def checkOutput(program: String, expected: String): Unit = {
    assertEquals(
      parser.parseProgram.parseAll(program).flatMap(compile).map(getOutput),
      Right(expected)
    )
  }

}
