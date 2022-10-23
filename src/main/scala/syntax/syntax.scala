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
  case UnOp(primOp: UnPrim, e: Exp)
  case BinOp(primOp: BinPrim, e1: Exp, e2: Exp)
  case If(test: Exp, thenB: Exp, elseB: Exp)
  case Let(bindings: List[(Name, Exp)], body: Exp)
  case App(lvar: Name, args: List[Exp])
}

final case class FunDef(lvar: Name, formals: List[Name], body: Exp)

final case class Program(funs: List[FunDef], body: Exp)
