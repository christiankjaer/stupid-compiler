package parser

import cats.parse.{Caret, Parser => P, Parser0 => P0}

val comment: P[Unit] =
  P.char('#') *> P.until0(P.char('\n')).void

val whitespace: P[Unit] = P.charIn(" \t\n").void | comment
val whitespaces0: P0[Unit] = whitespace.rep0.void

def token[T](p: P[T]): P[T] =
  p <* whitespaces0

def locToken[T](p: P[T]): P[(T, SourceLocation)] =
  (P.caret.with1 ~ p ~ P.caret).map { case ((begin, res), end) =>
    (res, SourceLocation(begin, end))
  } <* whitespaces0

val identifier: P[String] =
  (P.charIn('a' to 'z') *>
    (P.charIn('a' to 'z') |
      P.charIn('A' to 'Z') |
      P.charIn('0' to '9') |
      P.char('_')).rep0).string

val notAfterKeyword: P[Char] = P.charIn('a' to 'z') | P.charIn('A' to 'Z') | P.charIn('0' to '9')
def keyword(w: String) = token(P.string(w) <* P.not(P.peek(notAfterKeyword)))
def locKeyword(w: String): P[SourceLocation] =
  locToken(P.string(w) <* P.not(P.peek(notAfterKeyword))).map(_(1))

private def parens[T](p: P0[T]): P[T] =
  token(P.char('(')) *> p <* token(P.char(')'))

private def locParens[T](p: P0[T]): P[(SourceLocation, T, SourceLocation)] =
  (locToken(P.char('(')) ~ p ~ locToken(P.char(')'))).map { case ((((), begin), res), ((), end)) =>
    (begin, res, end)
  }
