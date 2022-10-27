package syntax

enum Builtin {
  case Zeroary(e: Exp)
  case Unary(f: Exp => Exp)
  case Binary(f: (Exp, Exp) => Exp)
}

val builtins: Map[Name, Builtin] = Map(
  "print" -> Builtin.Unary(e => Exp.UnOp(UnPrim.Print, e)),
  "is_zero" -> Builtin.Unary(e => Exp.UnOp(UnPrim.IsZero, e)),
  "is_unit" -> Builtin.Unary(e => Exp.UnOp(UnPrim.IsUnit, e)),
  "is_int" -> Builtin.Unary(e => Exp.UnOp(UnPrim.IsInt, e)),
  "is_bool" -> Builtin.Unary(e => Exp.UnOp(UnPrim.IsBool, e)),
  "is_char" -> Builtin.Unary(e => Exp.UnOp(UnPrim.IsChar, e)),
  "int_to_char" -> Builtin.Unary(e => Exp.UnOp(UnPrim.IntToChar, e)),
  "char_to_int" -> Builtin.Unary(e => Exp.UnOp(UnPrim.CharToInt, e))
)
