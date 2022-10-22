package parser

import cats.parse.Rfc5234.{alpha, digit}
import cats.parse.{Parser0, Parser => P, Numbers}
import cats.syntax.all.*

import syntax.*

/*

Syntax:

program :

fun f(x, y, x) = exp
fun g() = exp

in
  let x = foo
      y = bar
      z = baz
  in x + y + z

 */

val parseExp: P[Exp] = P.recursive[Exp] { expr =>

  val app = (token(identifier) ~ expr
    .repSep0(token(P.char(',')))
    .between(token(P.char('(')), token(P.char(')')))).map(Exp.App.apply)

  val base = app.backtrack | token(identifier).map(Exp.Var.apply) |
    token(
      Numbers.nonNegativeIntString.map(x => Exp.CExp(Const.Fixnum(x.toInt)))
    )
    | token(P.string("()")).as(Exp.CExp(Const.Null))
    | expr.between(token(P.char('(')), token(P.char(')')))


  def factor: P[Exp] =
    (token(P.char('-')) *> P.defer(factor))
      .map(Exp.UnOp(UnPrim.Neg, _)) | base

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

  //def let =
  arith ~ (eq *> arith).? map {
    case (e1, None)     => e1
    case (e1, Some(e2)) => Exp.BinOp(BinPrim.Eq, e1, e2)
  }

  //(token(P.string("let")) *> (token(identifier).soft ~ (token(
  //  P.char('=')
  //) *> let)).rep ~ (token(P.string("in")) *> expr)).map {
  //  case (bindings, body) => Exp.Let(bindings.toList, body)
  //} | let

}

val parseFunDef: P[FunDef] =
  (token(P.string("fun")) *> (token(identifier) ~ token(identifier)
    .repSep0(token(P.char(',')))
    .between(P.char('('), P.char(')')))
    ~ (token(P.char('=')) *> parseExp)).map { case ((f, args), body) =>
    FunDef(f, args, body)
  }

val parseProgram: P[Program] =
  (parseFunDef.rep ~ (token(P.string("in")) *> parseExp))
    .map { case (defs, body) => Program(defs.toList, body) } | parseExp.map(e =>
    Program(List.empty, e)
  )
