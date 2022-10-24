package parser

import cats.parse.{Parser => P, Parser0 => P0}

val comment: P[Unit] =
  P.char('#') *> P.until0(P.char('\n')).void

val whitespace: P0[Unit] = (P.charIn(" \t\n").void | comment).rep0.void

def token[T](p: P[T]): P[T] =
  p <* whitespace

val identifier: P[String] =
  (P.charIn('a' to 'z') *>
    (P.charIn('a' to 'z') |
      P.charIn('A' to 'Z') |
      P.charIn('0' to '9') |
      P.char('_')).rep0).string

def keyword(w: String) = token(P.string(w))
