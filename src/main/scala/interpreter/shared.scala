package interpreter

import cats.data.Kleisli
import syntax.*

type Error = String

final case class Env(globals: Map[Name, Binding], locals: Map[Name, Binding]) {
  def extendLocal(name: Name, v: Const): Env =
    copy(locals = locals + (name -> Binding.Variable(v)))
}

// Interpreter monad
type Interp[T] = Kleisli[Either[Error, *], Env, T]

enum Binding {
  case Variable(c: Const)
  case Function(formals: List[Name], body: Exp)
  case Toplevel(b: Builtin)
}

def lookup(n: Name): Interp[Option[Binding]] =
  Kleisli.ask.map(env => env.locals.get(n) orElse env.globals.get(n))

def pure[T](a: T): Interp[T] =
  Kleisli.pure(a)

def err[T](e: Error): Interp[T] =
  Kleisli.liftF(Left(e))
