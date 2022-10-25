package interpreter

import syntax.*

val baseEnv: Map[Name, Binding] =
  builtins.map((k, v) => k -> Binding.Toplevel(v))

def interpProgram(p: Program): Either[Error, Const] = {

  val funs: Map[Name, Binding] =
    p.funs.map(fd => fd.name -> Binding.Function(fd.formals, fd.body)).toMap

  val env: Env = Env(baseEnv ++ funs, Map.empty)

  interpExp(p.body).run(env)
}
