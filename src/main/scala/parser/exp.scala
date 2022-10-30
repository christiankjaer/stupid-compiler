package parser

import cats.parse.Rfc5234.{alpha, char, digit}
import cats.parse.{Numbers, Parser => P, Parser0}
import cats.syntax.all.*
import syntax.*

val parseConst: P[(Const, SourceLocation)] =
  locKeyword("false").map(loc => (Const.Bool(false), loc))
    | locKeyword("true").map(loc => (Const.Bool(true), loc))
    | locToken(P.string("()")).map(x => (Const.Unit, x(1)))
    | locToken(Numbers.signedIntString).map { case (num, loc) => (Const.Int(num.toInt), loc) }
    | locToken(char.between(P.char('\''), P.char('\''))).map(c => (Const.Ch(c(0)), c(1)))

private val neg: P[UnPrim] = token(P.char('~')).as(UnPrim.Neg)
private val not: P[UnPrim] = token(P.char('!')).as(UnPrim.Not)
private val plus: P[BinPrim] = token(P.char('+')).as(BinPrim.Plus)
private val times: P[BinPrim] = token(P.char('*')).as(BinPrim.Times)
private val minus: P[BinPrim] = token(P.char('-')).as(BinPrim.Minus)
private val div: P[BinPrim] = token(P.char('/')).as(BinPrim.Div)
private val eq: P[BinPrim] = token(P.string("==")).as(BinPrim.Eq)

type LExp = Exp[SourceLocation]

val parseExp: P[LExp] = P.recursive[LExp] { expr =>

  val app = (P.caret.with1 ~ token(identifier) ~ locParens(expr.repSep0(token(P.char(','))))).map {
    case ((start, id), (_, args, end)) =>
      Exp.App(id, args, end.copy(begin = start))
  }

  val base = parseConst.map { case (c, loc) => Exp.C(c, loc) }.backtrack
    | app.backtrack
    | locToken(identifier).map { case (x, loc) => Exp.Var(x, loc) }
    | parens(expr)

  def factor: P[LExp] = (P.caret.with1 ~ (neg | not) ~ P.defer(factor)).map { case ((pos, op), e) =>
    Exp.UnOp(op, e, SourceLocation(pos, e.ann.end))
  } | base

  def term1: Parser0[LExp => LExp] =
    ((times | div) ~ factor ~ P.defer0(term1)).map { case ((op, b), f) =>
      (a: LExp) => f(Exp.BinOp(op, a, b, a.ann |+| b.ann))
    } | P.pure((x: LExp) => x)

  def term: P[LExp] = (factor ~ term1).map { case (a, f) => f(a) }

  def arith1: Parser0[LExp => LExp] =
    ((plus | minus) ~ term ~ P.defer0(arith1)).map { case ((op, b), f) =>
      (a: LExp) => f(Exp.BinOp(op, a, b, a.ann |+| b.ann))
    } | P.pure((x: LExp) => x)

  def arith: P[LExp] = (term ~ arith1).map { case (a, f) => f(a) }

  def ar = arith ~ (eq ~ arith).? map {
    case (e1, None)           => e1
    case (e1, Some((op, e2))) => Exp.BinOp(op, e1, e2, e1.ann |+| e2.ann)
  }

  def iff = (((locKeyword("if") ~ expr) <* keyword("then")) ~
    (expr <* keyword("else")) ~ expr).map { case (((start, test), th), el) =>
    Exp.If(test, th, el, start |+| el.ann)
  } | ar

  (locKeyword("let") ~ (token(identifier).soft ~ (token(P.char('=')) *> iff)).rep ~
    (keyword("in") *> expr)).map { case ((start, bindings), body) =>
    Exp.Let(bindings.toList, body, start |+| body.ann)
  } | iff
}
