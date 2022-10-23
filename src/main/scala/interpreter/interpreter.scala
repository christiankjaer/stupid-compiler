package interpreter

import syntax.*

type Error = String

// Interpreter monad
type I[T] = Either[Error, T]

enum Binding {
  case Variable(c: Const)
  case Function(formals: List[Name], body: Exp)
  case BIn(b: Builtin)
}

type Env = Map[Name, Binding]

val baseEnv: Map[Name, Binding] =
  builtins.map((k, v) => k -> Binding.BIn(v))

def interpUnOp(op: UnPrim, v: Const): I[Const] = (op, v) match
  case (UnPrim.CharToInt, Const.Ch(c)) => Right(Const.Int(c.intValue))

  case (UnPrim.IntToChar, Const.Int(i)) => Right(Const.Ch(i.toChar))

  case (UnPrim.IsZero, c) => Right(Const.Bool(c == Const.Int(0)))

  case (UnPrim.IsUnit, c) => Right(Const.Bool(c == Const.Unit))

  case (UnPrim.Not, Const.Bool(false)) => Right(Const.Bool(true))
  case (UnPrim.Not, _)                 => Right(Const.Bool(false))

  case (UnPrim.IsInt, Const.Int(_)) => Right(Const.Bool(true))
  case (UnPrim.IsInt, _)            => Right(Const.Bool(false))

  case (UnPrim.IsBool, Const.Bool(_)) => Right(Const.Bool(true))
  case (UnPrim.IsBool, _)             => Right(Const.Bool(false))

  case (UnPrim.IsChar, Const.Ch(_)) => Right(Const.Bool(true))
  case (UnPrim.IsChar, _)           => Right(Const.Bool(false))

  case (UnPrim.Neg, Const.Int(i)) => Right(Const.Int(-i))

  case _ => Left("Type error")

def interpBinOp(op: BinPrim, v1: Const, v2: Const): I[Const] =
  (op, v1, v2) match
    case (BinPrim.Plus, Const.Int(i1), Const.Int(i2)) =>
      Right(Const.Int(i1 + i2))
    case (BinPrim.Minus, Const.Int(i1), Const.Int(i2)) =>
      Right(Const.Int(i1 - i2))
    case (BinPrim.Times, Const.Int(i1), Const.Int(i2)) =>
      Right(Const.Int(i1 * i2))
    case (BinPrim.Div, Const.Int(i1), Const.Int(i2)) =>
      Right(Const.Int(i1 / i2))
    case (BinPrim.Eq, _, _) =>
      Right(Const.Bool(v1 == v2))
    case _ => Left("Type error")

def interpLet(
    topDefs: Env,
    env: Env,
    bindings: List[(Name, Exp)],
    body: Exp
): I[Const] =
  bindings match
    case (name, exp) :: bs =>
      for {
        v <- interpExp(topDefs, env, exp)
        res <- interpLet(topDefs, env + (name -> Binding.Variable(v)), bs, body)
      } yield res
    case Nil => interpExp(topDefs, env, body)

def interpApp(
    topDefs: Env,
    env: Env,
    formals: List[Name],
    args: List[Exp],
    body: Exp
): I[Const] = {

  def go(newEnv: Env, params: List[(Name, Exp)]): I[Const] = {
    params match
      case (name, exp) :: ps =>
        for {
          v <- interpExp(topDefs, env, exp)
          res <- go(newEnv + (name -> Binding.Variable(v)), ps)
        } yield res
      case Nil =>
        interpExp(topDefs, newEnv, body)
  }

  if formals.length != args.length
  then Left("Wrong number of arguments")
  else go(Map.empty, formals.zip(args))

}

def interpExp(topDefs: Env, env: Env, e: Exp): I[Const] = e match
  case Exp.Var(x) =>
    (topDefs ++ env).get(x) match
      case Some(Binding.Variable(c)) => Right(c)
      case _                         => Left(s"Unbound variable $x")

  case Exp.CExp(c) => Right(c)
  case Exp.UnOp(primOp, e) =>
    interpExp(topDefs, env, e).flatMap(interpUnOp(primOp, _))
  case Exp.BinOp(primOp, e1, e2) =>
    for {
      v1 <- interpExp(topDefs, env, e1)
      v2 <- interpExp(topDefs, env, e2)
      res <- interpBinOp(primOp, v1, v2)
    } yield res

  case Exp.If(test, thenB, elseB) =>
    for {
      t <- interpExp(topDefs, env, test)
      conseq <- t match
        case Const.Bool(false) => interpExp(topDefs, env, elseB)
        case _                 => interpExp(topDefs, env, thenB)
    } yield conseq

  case Exp.Let(bindings, body) => interpLet(topDefs, env, bindings, body)

  case Exp.App(lvar, args) =>
    ((topDefs ++ env).get(lvar), args) match
      case (Some(Binding.Function(formals, body)), _) =>
        interpApp(topDefs, env, formals, args, body)
      case (Some(Binding.BIn(Builtin.Unary(f))), List(e)) =>
        interpExp(topDefs, env, f(e))
      case (Some(Binding.BIn(Builtin.Binary(f))), List(e1, e2)) =>
        interpExp(topDefs, env, f(e1, e2))
      case _ => Left(s"Unknown function $lvar")

def interpProgram(p: Program): I[Const] = {

  val bindings: Env =
    p.funs.map(fd => fd.lvar -> Binding.Function(fd.formals, fd.body)).toMap

  interpExp(baseEnv ++ bindings, Map.empty, p.body)
}
