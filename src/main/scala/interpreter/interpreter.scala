package interpreter

import cats.data.Kleisli
import syntax.*

type Error = String

final case class Env(globals: Map[Name, Binding], locals: Map[Name, Binding]) {
  def extendLocal(name: Name, v: Const): Env =
    copy(locals = locals + (name -> Binding.Variable(v)))
}

// Interpreter monad
type I[T] = Kleisli[Either[Error, *], Env, T]

enum Binding {
  case Variable(c: Const)
  case Function(formals: List[Name], body: Exp)
  case Toplevel(b: Builtin)
}

def lookup(n: Name): I[Option[Binding]] =
  Kleisli.ask.map(env => env.locals.get(n) orElse env.globals.get(n))

def pure[T](a: T): I[T] =
  Kleisli.pure(a)

def err[T](e: Error): I[T] =
  Kleisli.liftF(Left(e))

def interpUnOp(op: UnPrim, v: Const): I[Const] = (op, v) match
  case (UnPrim.CharToInt, Const.Ch(c)) => pure(Const.Int(c.intValue))

  case (UnPrim.IntToChar, Const.Int(i)) => pure(Const.Ch(i.toChar))

  case (UnPrim.IsZero, c) => pure(Const.Bool(c == Const.Int(0)))

  case (UnPrim.IsUnit, c) => pure(Const.Bool(c == Const.Unit))

  case (UnPrim.Not, Const.Bool(false)) => pure(Const.Bool(true))
  case (UnPrim.Not, _)                 => pure(Const.Bool(false))

  case (UnPrim.IsInt, Const.Int(_)) => pure(Const.Bool(true))
  case (UnPrim.IsInt, _)            => pure(Const.Bool(false))

  case (UnPrim.IsBool, Const.Bool(_)) => pure(Const.Bool(true))
  case (UnPrim.IsBool, _)             => pure(Const.Bool(false))

  case (UnPrim.IsChar, Const.Ch(_)) => pure(Const.Bool(true))
  case (UnPrim.IsChar, _)           => pure(Const.Bool(false))

  case (UnPrim.Neg, Const.Int(i)) => pure(Const.Int(-i))

  case _ => err("Type error")

def interpBinOp(op: BinPrim, v1: Const, v2: Const): I[Const] =
  (op, v1, v2) match
    case (BinPrim.Plus, Const.Int(i1), Const.Int(i2)) =>
      pure(Const.Int(i1 + i2))
    case (BinPrim.Minus, Const.Int(i1), Const.Int(i2)) =>
      pure(Const.Int(i1 - i2))
    case (BinPrim.Times, Const.Int(i1), Const.Int(i2)) =>
      pure(Const.Int(i1 * i2))
    case (BinPrim.Div, Const.Int(i1), Const.Int(i2)) =>
      pure(Const.Int(i1 / i2))
    case (BinPrim.Eq, _, _) =>
      pure(Const.Bool(v1 == v2))
    case _ => err("Type error")

def interpLet(
    bindings: List[(Name, Exp)],
    body: Exp
): I[Const] =
  bindings match
    case (name, exp) :: bs =>
      for {
        v <- interpExp(exp)
        res <- interpLet(bs, body).local[Env](_.extendLocal(name, v))
      } yield res
    case Nil => interpExp(body)

def interpApp(
    formals: List[Name],
    args: List[Exp],
    body: Exp
): I[Const] = {

  def go(newLocals: Map[Name, Binding], params: List[(Name, Exp)]): I[Const] = {
    params match
      case (name, exp) :: ps =>
        for {
          v <- interpExp(exp)
          res <- go(newLocals + (name -> Binding.Variable(v)), ps)
        } yield res
      case Nil => interpExp(body).local(_.copy(locals = newLocals))
  }

  if formals.length != args.length
  then err("Wrong number of arguments")
  else go(Map.empty, formals.zip(args))
}

def interpExp(e: Exp): I[Const] = e match
  case Exp.Var(x) =>
    lookup(x).flatMap {
      case Some(Binding.Variable(c)) => pure(c)
      case _                         => err(s"Unbound variable $x")
    }

  case Exp.CExp(c) => pure(c)
  case Exp.UnOp(primOp, e) =>
    interpExp(e).flatMap(interpUnOp(primOp, _))
  case Exp.BinOp(primOp, e1, e2) =>
    for {
      v1 <- interpExp(e1)
      v2 <- interpExp(e2)
      res <- interpBinOp(primOp, v1, v2)
    } yield res

  case Exp.If(test, thenB, elseB) =>
    for {
      t <- interpExp(test)
      conseq <- t match
        case Const.Bool(false) => interpExp(elseB)
        case _                 => interpExp(thenB)
    } yield conseq

  case Exp.Let(bindings, body) => interpLet(bindings, body)

  case Exp.App(lvar, args) =>
    lookup(lvar).flatMap {
      (_, args) match {
        case (Some(Binding.Function(formals, body)), _) =>
          interpApp(formals, args, body)
        case (Some(Binding.Toplevel(Builtin.Unary(f))), List(e)) =>
          interpExp(f(e))
        case (Some(Binding.Toplevel(Builtin.Binary(f))), List(e1, e2)) =>
          interpExp(f(e1, e2))
        case _ => err(s"Unknown function $lvar")
      }
    }

val baseEnv: Map[Name, Binding] =
  builtins.map((k, v) => k -> Binding.Toplevel(v))

def interpProgram(p: Program): Either[Error, Const] = {

  val funs: Map[Name, Binding] =
    p.funs.map(fd => fd.name -> Binding.Function(fd.formals, fd.body)).toMap

  val env: Env = Env(baseEnv ++ funs, Map.empty)

  interpExp(p.body).run(env)
}
