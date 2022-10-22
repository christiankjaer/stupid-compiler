type Name = String

enum UnPrim {
  case Inc, Dec, CharToFixnum, FixnumToChar, IsZero, IsNull, Not, IsFixnum,
    IsBool, IsChar
}
enum BinPrim {
  case Plus
}
enum Const {
  case Fixnum(n: Long)
  case Ch(n: Char)
  case True, False, Null
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

final case class Lambda(formals: List[Name], body: Exp)

final case class FunDef(lvar: Name, lam: Lambda)

final case class Program(funs: List[FunDef], body: Exp)
