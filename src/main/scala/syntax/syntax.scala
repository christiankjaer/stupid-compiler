package syntax

type Name = String

enum UnPrim {
  case CharToInt, IntToChar, IsZero, IsUnit, Not, IsInt,
    IsBool, IsChar, Neg
}
enum BinPrim {
  case Plus, Minus, Times, Div, Eq
}
enum Const {
  case Int(n: Long)
  case Ch(n: Char)
  case Bool(b: Boolean)
  case Unit
}

enum Exp {
  case Var(x: Name)
  case CExp(c: Const)
  case UnOp(prim: UnPrim, e: Exp)
  case BinOp(prim: BinPrim, e1: Exp, e2: Exp)
  case If(test: Exp, thenB: Exp, elseB: Exp)
  case Let(bindings: List[(Name, Exp)], body: Exp)
  case App(fun: Name, args: List[Exp])
}

final case class FunDef(name: Name, formals: List[Name], body: Exp)

final case class Program(funs: List[FunDef], body: Exp)
