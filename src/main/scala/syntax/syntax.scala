package syntax

type Name = String

enum UnPrim {
  case CharToInt, IntToChar, IsZero, IsUnit, Not, IsInt,
    IsBool, IsChar, Neg
}
enum BinPrim {
  case Plus, Minus, Times, Div, Eq, Lt, Le, Gt, Ge
}
enum Const {
  case Int(n: Long)
  case Ch(n: Char)
  case Bool(b: Boolean)
  case Unit
}

enum Exp[A] {

  val ann: A

  case Var(x: Name, ann: A)
  case C(c: Const, ann: A)
  case UnOp(prim: UnPrim, e: Exp[A], ann: A)
  case BinOp(prim: BinPrim, e1: Exp[A], e2: Exp[A], ann: A)
  case If(test: Exp[A], thenB: Exp[A], elseB: Exp[A], ann: A)
  case Let(bindings: List[(Name, Exp[A])], body: Exp[A], ann: A)
  case App(fun: Name, args: List[Exp[A]], ann: A)

  def map[B](f: A => B): Exp[B] = this match
    case Exp.Var(x, ann)                 => Var(x, f(ann))
    case Exp.C(c, ann)                   => C(c, f(ann))
    case Exp.UnOp(prim, e, ann)          => UnOp(prim, e.map(f), f(ann))
    case Exp.BinOp(prim, e1, e2, ann)    => BinOp(prim, e1.map(f), e2.map(f), f(ann))
    case Exp.If(test, thenB, elseB, ann) => If(test.map(f), thenB.map(f), elseB.map(f), f(ann))
    case Exp.Let(bindings, body, ann) =>
      Let(bindings.map { case (name, e) => (name, e.map(f)) }, body.map(f), f(ann))
    case Exp.App(fun, args, ann) => App(fun, args.map(_.map(f)), f(ann))

}

final case class FunDef[A](name: Name, formals: List[Name], body: Exp[A]) {
  def map[B](f: A => B): FunDef[B] = copy(body = body.map(f))
}

final case class Program[A](funs: List[FunDef[A]], body: Exp[A]) {
  def map[B](f: A => B): Program[B] = Program(funs.map(_.map(f)), body.map(f))
}
