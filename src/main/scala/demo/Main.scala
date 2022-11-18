package demo

import cats.syntax.all.*
import com.monovore.decline.{CommandApp, Opts}
import compiler.*
import parser.*

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Path}
import scala.io.Source

import sys.process.*

val name = Opts.argument[Path](metavar = "name")

object Main
    extends CommandApp(
      name = "stupid",
      header = "Compiles stupid programs",
      main = name.map(compileProgram)
    )

def compileProgram(outputFile: Path): Unit = {
  val program = """
    .text
    .globl  program_entry
    .type   program_entry, @function
program_entry:
    push %rbp
    movq %rsp, %rbp
    movq $1337, %rax
    pop %rbp
    ret
  """
  val p = Files.createTempFile("program", ".s")
  val bw = new FileWriter(p.toFile())
  bw.append(program)
  bw.close()

  Process(
    Seq("zig", "build", s"-Dentry=${p.toString}"),
    new File("./runtime")
  ).!
  Process(
    Seq("cp", "./runtime/zig-out/bin/runtime", outputFile.toString)
  ).!
}
