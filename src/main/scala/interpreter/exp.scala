package interpreter

import cats.data.Kleisli
import syntax.*

def interpUnOp(op: UnPrim, v: Const): Interp[Const] = (op, v) match
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

  case (UnPrim.Print, v) => println(constToString(v)) ; pure(Const.Unit)

  case _ => err("Type error")

def interpBinOp(op: BinPrim, v1: Const, v2: Const): Interp[Const] =
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

def interpLet(bindings: List[(Name, Exp)], body: Exp): Interp[Const] =
  bindings match
    case (name, exp) :: bs =>
      for {
        v <- interpExp(exp)
        res <- interpLet(bs, body).local[Env](_.extendLocal(name, v))
      } yield res
    case Nil => interpExp(body)

def interpApp(formals: List[Name], args: List[Exp], body: Exp): Interp[Const] = {

  def go(newLocals: Map[Name, Binding], params: List[(Name, Exp)]): Interp[Const] = {
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

def interpExp(e: Exp): Interp[Const] = e match
  case Exp.Var(x) =>
    lookup(x).flatMap {
      case Some(Binding.Variable(c)) => pure(c)
      case _                         => err(s"Unbound variable $x")
    }

  case Exp.C(c) => pure(c)
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
        case (Some(Binding.Toplevel(Builtin.Zeroary(e))), Nil) =>
          interpExp(e)
        case (Some(Binding.Toplevel(Builtin.Unary(f))), List(e)) =>
          interpExp(f(e))
        case (Some(Binding.Toplevel(Builtin.Binary(f))), List(e1, e2)) =>
          interpExp(f(e1, e2))
        case _ => err(s"Unknown function $lvar")
      }
    }
