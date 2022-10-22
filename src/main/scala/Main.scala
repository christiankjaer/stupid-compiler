import parser.*
import compiler.*
import com.monovore.decline.CommandApp
import com.monovore.decline.Opts
import java.nio.file.Path
import cats.syntax.all.*
import java.nio.file.Files
import java.io.FileWriter
import scala.io.Source
import sys.process.*
import java.nio.file.Files
import java.io.BufferedWriter
import java.io.File


val inputFile = Opts.argument[Path](metavar = "file")
val name = Opts.argument[Path](metavar = "name")

def compileProgram(inputFile: Path, outputFile: Path): Unit = {

  val inputProgram = Source.fromFile(inputFile.toFile()).getLines().mkString
  val compiled = parseProgram.parseAll(inputProgram).leftMap(_ => "Parse error").flatMap(compile) match
    case Right(p) => p
    case Left(e) => sys.error(e)

  val p = Files.createTempFile("program", ".s")
  val file = p.toFile()
  val bw = new FileWriter(file)
  bw.append(compiled)
  bw.close()

  val cmd = Process(
    Seq("zig", "build", s"-Dentry=${p.toString}"), 
    new File("./runtime")
  )
  cmd.!
  Process(
    Seq("cp", "./runtime/zig-out/bin/runtime", outputFile.toString),
  ).!
}

object Main extends CommandApp(
  name = "stupid",
  header = "Compiles stupid programs",
  main = (inputFile, name).mapN(compileProgram)

)