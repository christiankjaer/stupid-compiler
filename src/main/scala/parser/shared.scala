package parser

import cats.kernel.Semigroup
import cats.parse.Caret

final case class SourceLocation(begin: Caret, end: Caret) {}
object SourceLocation {
  val start: SourceLocation =
    SourceLocation(Caret.Start, Caret.Start)
}

given Semigroup[SourceLocation] with {
  override def combine(x: SourceLocation, y: SourceLocation): SourceLocation =
    SourceLocation(x.begin, y.end)

}
