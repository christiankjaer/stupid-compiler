package parser

import cats.parse.{Parser => P, Parser0 => P0}

def token[T](p: P[T]): P[T] =
  p.surroundedBy(P.charIn(" \t\n").rep0)

val identifier: P[String] =
  (P.charIn('a' to 'z') *>
    (P.charIn('a' to 'z') |
      P.charIn('A' to 'Z') |
      P.charIn('0' to '9') |
      P.char('_')).rep0).string
