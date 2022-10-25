package compiler

import cats.data.{EitherT, State}
import cats.syntax.all.*
import syntax.*

enum Binding {
  case StackPos(i: Int)
  case ProgramLabel(l: Label, arity: Int)
  case Toplevel(b: Builtin)
}

type Instruction = String
type Label = String
type Env = Map[Name, Binding]
type Error = String

type LabelCounter = Int

// Compile monad
type Compile[T] = EitherT[State[LabelCounter, _], Error, T]

def error[T](e: Error): Compile[T] =
  EitherT.leftT(e)

def pure[T](a: T): Compile[T] = EitherT.pure(a)

def makeLabel: Compile[Label] = for {
  lab <- EitherT.liftF(State.get)
  _ <- EitherT.liftF(State.set(lab + 1))
} yield s"L_$lab"
