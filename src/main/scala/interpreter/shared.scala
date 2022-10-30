package interpreter

import cats.data.Kleisli
import parser.SourceLocation
import syntax.*

final case class Error(msg: String, loc: SourceLocation)

final case class Env(globals: Map[Name, Binding], locals: Map[Name, Binding]) {
  def extendLocal(name: Name, v: Const): Env =
    copy(locals = locals + (name -> Binding.Variable(v)))
}

// Interpreter monad
type Interp[T] = Kleisli[Either[Error, *], Env, T]

enum Binding {
  case Variable(c: Const)
  case Function(formals: List[Name], body: LExp)
  case Toplevel(b: Builtin[SourceLocation])
}

def lookup(n: Name): Interp[Option[Binding]] =
  Kleisli.ask.map(env => env.locals.get(n) orElse env.globals.get(n))

def pure[T](a: T): Interp[T] =
  Kleisli.pure(a)

def err[T](msg: String, loc: SourceLocation): Interp[T] =
  Kleisli.liftF(Left(Error(msg, loc)))
