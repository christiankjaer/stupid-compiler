package compiler

import cats.data.{EitherT, State}
import cats.syntax.all.*
import parser.SourceLocation
import syntax.*

enum Binding {
  case StackPos(i: Int)
  case ProgramLabel(l: Label, arity: Int)
  case Toplevel(b: Builtin[SourceLocation])
}

type Instruction = String
type Label = String
type Env = Map[Name, Binding]
final case class Error(msg: String, loc: SourceLocation) {
  override def toString(): String = s"$msg at line: ${loc.begin.line}, col: ${loc.begin.col}"
}

type LabelCounter = Int

// Compile monad
type Compile[T] = EitherT[State[LabelCounter, _], Error, T]

def error[T](msg: String, loc: SourceLocation): Compile[T] =
  EitherT.leftT(Error(msg, loc))

def pure[T](a: T): Compile[T] = EitherT.pure(a)

def makeLabel: Compile[Label] = for {
  lab <- EitherT.liftF(State.get)
  _ <- EitherT.liftF(State.set(lab + 1))
} yield s"L_$lab"
