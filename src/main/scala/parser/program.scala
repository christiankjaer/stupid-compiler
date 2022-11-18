package parser

import cats.parse.{Parser => P}
import syntax.*

val parseFunDef: P[FunDef[SourceLocation]] =
  (keyword("fun") *>
    (token(identifier) ~ parens(token(identifier).repSep0(comma))) ~
    (assign *> parseExp)).map { case ((f, args), body) =>
    FunDef(f, args, body)
  }

val parseFunDefs: P[Program[SourceLocation]] =
  (parseFunDef.rep ~ (keyword("in") *> parseExp)).map { case (defs, body) => Program(defs.toList, body) }

val parseProgram: P[Program[SourceLocation]] =
  whitespaces0.with1 *> (parseFunDefs | parseExp.map(e => Program(List.empty, e)))
