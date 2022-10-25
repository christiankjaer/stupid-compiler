package parser

import cats.parse.Rfc5234.{alpha, char, digit}
import cats.parse.{Numbers, Parser => P, Parser0}
import syntax.*

val parseConst: P[Const] =
  keyword("false").as(Const.Bool(false))
    | keyword("true").as(Const.Bool(true))
    | token(P.string("()")).as(Const.Unit)
    | token(Numbers.signedIntString.map(x => Const.Int(x.toInt)))
    | token(char.between(P.char('\''), P.char('\''))).map(c => Const.Ch(c))

val parseExp: P[Exp] = P.recursive[Exp] { expr =>

  val app = (token(identifier) ~ expr
    .repSep0(token(P.char(',')))
    .between(token(P.char('(')), token(P.char(')')))).map(Exp.App.apply)

  val base = parseConst.map(Exp.CExp.apply).backtrack
    | app.backtrack
    | token(identifier).map(Exp.Var.apply)
    | expr.between(token(P.char('(')), token(P.char(')')))

  def factor: P[Exp] =
    (token(P.charIn('~', '!')) ~ P.defer(factor)).map {
      case ('~', e) => Exp.UnOp(UnPrim.Neg, e)
      case (_, e)   => Exp.UnOp(UnPrim.Not, e)
    } | base

  val plus: P[(Exp, Exp) => Exp] =
    token(P.char('+').as(Exp.BinOp(BinPrim.Plus, _, _)))
  val times: P[(Exp, Exp) => Exp] =
    token(P.char('*').as(Exp.BinOp(BinPrim.Times, _, _)))
  val minus: P[(Exp, Exp) => Exp] =
    token(P.char('-').as(Exp.BinOp(BinPrim.Minus, _, _)))
  val div: P[(Exp, Exp) => Exp] =
    token(P.char('/').as(Exp.BinOp(BinPrim.Div, _, _)))
  val eq: P[Unit] = token(P.string("=="))

  def term1: Parser0[Exp => Exp] =
    ((times | div) ~ factor ~ P.defer0(term1)).map { case ((op, b), f) =>
      (a: Exp) => f(op(a, b))
    } | P.pure((x: Exp) => x)

  def term: P[Exp] = (factor ~ term1).map { case (a, f) => f(a) }

  def arith1: Parser0[Exp => Exp] =
    ((plus | minus) ~ term ~ P.defer0(arith1)).map { case ((op, b), f) =>
      (a: Exp) => f(op(a, b))
    } | P.pure((x: Exp) => x)

  def arith: P[Exp] = (term ~ arith1).map { case (a, f) => f(a) }

  def ar = arith ~ (eq *> arith).? map {
    case (e1, None)     => e1
    case (e1, Some(e2)) => Exp.BinOp(BinPrim.Eq, e1, e2)
  }

  def iff = ((keyword("if") *> expr <* keyword("then")) ~
    (expr <* keyword("else")) ~ expr).map { case ((test, th), el) =>
    Exp.If(test, th, el)
  } | ar

  (keyword("let") *> (token(identifier).soft ~ (token(P.char('=')) *> iff)).rep ~
    (keyword("in") *> expr)).map { case (bindings, body) =>
    Exp.Let(bindings.toList, body)
  } | iff
}