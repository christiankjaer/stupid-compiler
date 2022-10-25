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

private val neg: P[UnPrim] = token(P.char('~')).as(UnPrim.Neg)
private val not: P[UnPrim] = token(P.char('!')).as(UnPrim.Not)
private val plus: P[BinPrim] = token(P.char('+')).as(BinPrim.Plus)
private val times: P[BinPrim] = token(P.char('*')).as(BinPrim.Times)
private val minus: P[BinPrim] = token(P.char('-')).as(BinPrim.Minus)
private val div: P[BinPrim] = token(P.char('/')).as(BinPrim.Div)
private val eq: P[BinPrim] = token(P.string("==")).as(BinPrim.Eq)

val parseExp: P[Exp] = P.recursive[Exp] { expr =>

  val app = (token(identifier) ~ parens(expr.repSep0(token(P.char(','))))).map(Exp.App.apply)

  val base = parseConst.map(Exp.CExp.apply).backtrack
    | app.backtrack
    | token(identifier).map(Exp.Var.apply)
    | parens(expr)

  def factor: P[Exp] = ((neg | not) ~ P.defer(factor)).map(Exp.UnOp.apply) | base

  def term1: Parser0[Exp => Exp] =
    ((times | div) ~ factor ~ P.defer0(term1)).map { case ((op, b), f) =>
      (a: Exp) => f(Exp.BinOp(op, a, b))
    } | P.pure((x: Exp) => x)

  def term: P[Exp] = (factor ~ term1).map { case (a, f) => f(a) }

  def arith1: Parser0[Exp => Exp] =
    ((plus | minus) ~ term ~ P.defer0(arith1)).map { case ((op, b), f) =>
      (a: Exp) => f(Exp.BinOp(op, a, b))
    } | P.pure((x: Exp) => x)

  def arith: P[Exp] = (term ~ arith1).map { case (a, f) => f(a) }

  def ar = arith ~ (eq ~ arith).? map {
    case (e1, None)           => e1
    case (e1, Some((op, e2))) => Exp.BinOp(op, e1, e2)
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
