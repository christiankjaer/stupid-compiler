package compiler

import cats.data.{EitherT, State}
import cats.syntax.all.*
import syntax.*

val baseEnv: Map[Name, Binding] =
  builtins.map((k, v) => k -> Binding.Toplevel(v))

val prelude =
  List(
    "    .text",
    "    .globl    program_entry",
    "    .type     program_entry, @function"
  )

val entry = List(
  "program_entry:",
  s"    movq %rsp, %rcx", // Save original stack
  s"    leaq (%rdi, %rsi, ${wordSize}), %rsp"
)

val end = List(
  "    movq %rcx, %rsp", // Restore original stack
  "    ret"
)

final case class TopDef(label: Label, fd: FunDef)

def compileTopDef(
    env: Env,
    td: TopDef
): Compile[List[Instruction]] = {

  def go(env: Env, stackIdx: Int, formals: List[Name]): Compile[List[Instruction]] =
    formals match
      case f :: fs =>
        go(env + (f -> Binding.StackPos(stackIdx)), stackIdx - wordSize, fs)
      case Nil => compileExp(env, stackIdx, td.fd.body)

  go(env, -wordSize, td.fd.formals)
    .map(s"${td.label}: # fun ${td.fd.name}" :: _ ++ List("    ret"))
}

def compileProgram(p: Program): Compile[List[Instruction]] = {

  val topDefs = p.funs.traverse(f => makeLabel.map(l => TopDef(l, f)))

  for {
    defs <- topDefs
    initEnv = baseEnv ++ defs
      .map(x => x.fd.name -> Binding.ProgramLabel(x.label, x.fd.formals.length))
      .toMap
    funs <- defs.flatTraverse(td => compileTopDef(initEnv, td))
    bodyCode <- compileExp(initEnv, -wordSize, p.body)
  } yield prelude ++ funs ++ entry ++ bodyCode ++ end
}

def compile(p: Program): Either[Error, String] =
  compileProgram(p).value.runA(0).value.map(_.mkString("\n"))
