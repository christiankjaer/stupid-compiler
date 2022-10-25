package parser

import cats.parse.{Parser => P, Parser0 => P0}

val comment: P[Unit] =
  P.char('#') *> P.until0(P.char('\n')).void

val whitespace: P[Unit] = P.charIn(" \t\n").void | comment
val whitespaces0: P0[Unit] = whitespace.rep0.void
val whitespaces: P[Unit] = whitespace.rep.void

def token[T](p: P[T]): P[T] =
  p <* whitespaces0

val identifier: P[String] =
  (P.charIn('a' to 'z') *>
    (P.charIn('a' to 'z') |
      P.charIn('A' to 'Z') |
      P.charIn('0' to '9') |
      P.char('_')).rep0).string

val notAfterKeyword: P[Char] = P.charIn('a' to 'z') | P.charIn('A' to 'Z') | P.charIn('0' to '9')
def keyword(w: String) = token(P.string(w) <* P.not(P.peek(notAfterKeyword)))

private def parens[T](p: P0[T]): P[T] =
  token(P.char('(')) *> p <* token(P.char(')'))
