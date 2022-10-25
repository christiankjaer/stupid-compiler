package parser

import cats.parse.{Parser => P}
import syntax.*

val parseFunDef: P[FunDef] =
  (keyword("fun") *> (token(identifier) ~ token(identifier)
    .repSep0(token(P.char(',')))
    .between(token(P.char('(')), token(P.char(')'))))
    ~ (token(P.char('=')) *> parseExp)).map { case ((f, args), body) =>
    FunDef(f, args, body)
  }

val parseFunDefs: P[Program] =
  (parseFunDef.rep ~ (keyword("in") *> parseExp)).map { case (defs, body) => Program(defs.toList, body) }

val parseProgram: P[Program] =
  whitespace.with1 *> (parseFunDefs | parseExp.map(e => Program(List.empty, e)))
