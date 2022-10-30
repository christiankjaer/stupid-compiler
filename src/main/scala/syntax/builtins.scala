package syntax

enum Builtin[A] {
  case Zeroary(f: A => Exp[A])
  case Unary(f: (Exp[A], A) => Exp[A])
  case Binary(f: (Exp[A], Exp[A], A) => Exp[A])
}

def builtins[A]: Map[Name, Builtin[A]] = Map(
  "is_zero" -> Builtin.Unary((e, ann) => Exp.UnOp(UnPrim.IsZero, e, ann)),
  "is_unit" -> Builtin.Unary((e, ann) => Exp.UnOp(UnPrim.IsUnit, e, ann)),
  "is_int" -> Builtin.Unary((e, ann) => Exp.UnOp(UnPrim.IsInt, e, ann)),
  "is_bool" -> Builtin.Unary((e, ann) => Exp.UnOp(UnPrim.IsBool, e, ann)),
  "is_char" -> Builtin.Unary((e, ann) => Exp.UnOp(UnPrim.IsChar, e, ann)),
  "int_to_char" -> Builtin.Unary((e, ann) => Exp.UnOp(UnPrim.IntToChar, e, ann)),
  "char_to_int" -> Builtin.Unary((e, ann) => Exp.UnOp(UnPrim.CharToInt, e, ann))
)
