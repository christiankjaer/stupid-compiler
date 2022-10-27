package compiler

import cats.data.{EitherT, State}
import cats.syntax.all.*
import syntax.*

def constToImm(c: Const): String = c match
  case Const.Int(n)      => s"$$${n << intShift}"
  case Const.Bool(false) => s"$$${falseVal}"
  case Const.Bool(true)  => s"$$${trueVal}"
  case Const.Unit        => s"$$${unitVal}"
  case Const.Ch(c)       => s"$$${(c << charShift) | charTag}"

val setBoolean = List(
  "    sete %al",
  "    movzbq %al, %rax",
  s"    salq $$${boolShift}, %rax",
  s"    orq $$${boolTag}, %rax"
)

def compileUnPrim(p: UnPrim): List[Instruction] =
  p match
    case UnPrim.Neg =>
      List(
        "    negq %rax",
        "    and $0xFC, %al"
      )
    case UnPrim.CharToInt =>
      List(s"    sarq $$${charShift - intShift}, %rax")
    case UnPrim.IntToChar =>
      List(
        s"    salq $$${charShift - intShift}, %rax",
        s"    orq $$${charTag}, %rax"
      )
    case UnPrim.IsZero =>
      "    cmp $0, %rax" :: setBoolean
    case UnPrim.IsUnit =>
      s"    cmp $$${unitVal}, %rax" :: setBoolean
    case UnPrim.IsBool =>
      List(
        s"    and $$${boolMask}, %rax",
        s"    cmp $$${boolTag}, %rax"
      ) ++ setBoolean
    case UnPrim.IsInt =>
      List(
        s"    andq $$${intMask}, %rax",
        s"    cmp $$${intTag}, %rax"
      ) ++ setBoolean
    case UnPrim.IsChar =>
      List(
        s"    andq $$${charMask}, %rax",
        s"    cmp $$${charTag}, %rax"
      ) ++ setBoolean
    case UnPrim.Not =>
      s"    cmpq $$${falseVal}, %rax" :: setBoolean
    case UnPrim.Print =>
      List(
        "    movq %rax, %rdi",
      s"    addq $$${-wordSize - wordSize}, %rsp",
        "    callq print_const",
      s"    addq $$${wordSize}, %rsp"
      )

def compileBinPrim(stackIdx: Int, p: BinPrim): List[Instruction] = p match
  case BinPrim.Plus =>
    List(s"    addq ${stackIdx}(%rsp), %rax")
  case BinPrim.Minus =>
    List(
      s"    xchgq ${stackIdx}(%rsp), %rax", // There is definitely a better way
      s"    subq ${stackIdx}(%rsp), %rax"
    )
  case BinPrim.Times =>
    List(
      s"    sarq $$${intShift}, %rax",
      s"    imulq ${stackIdx}(%rsp), %rax"
    )
  case BinPrim.Div =>
    List(
      s"    xchgq ${stackIdx}(%rsp), %rax", // There is definitely a better way
      "    cqto",
      s"    idivq ${stackIdx}(%rsp), %rax",
      s"    salq $$${intShift}, %rax"
    )
  case BinPrim.Eq =>
    s"    cmpq ${stackIdx}(%rsp), %rax" :: setBoolean

def compileBinOp(env: Env, stackIdx: Int, bin: Exp.BinOp): Compile[List[Instruction]] =
  for {
    arg1 <- compileExp(env, stackIdx, bin.e1)
    arg2 <- compileExp(env, stackIdx - wordSize, bin.e2)
  } yield arg1 ++ List(
    s"    movq %rax, ${stackIdx}(%rsp)"
  ) ++ arg2 ++ compileBinPrim(stackIdx, bin.prim)

def compileIf(env: Env, stackIdx: Int, ifE: Exp.If): Compile[List[Instruction]] =
  for {
    altLabel <- makeLabel
    endLabel <- makeLabel
    testCode <- compileExp(env, stackIdx, ifE.test)
    thenCode <- compileExp(env, stackIdx, ifE.thenB)
    elseCode <- compileExp(env, stackIdx, ifE.elseB)
  } yield testCode ++ List(
    s"    cmp ${constToImm(Const.Bool(false))}, %al",
    s"    je $altLabel # jump to else"
  ) ++ thenCode ++ List(
    s"    jmp $endLabel",
    s"$altLabel: # else branch"
  ) ++ elseCode
    ++ List(s"$endLabel: # end of if")

def compileVar(env: Env, x: Name): Either[Error, List[Instruction]] =
  env.get(x) match
    case Some(Binding.StackPos(idx)) =>
      Right(List(s"    movq ${idx}(%rsp), %rax"))
    case _ => Left(s"Unknown variable '$x'")

def compileLet(
    env: Env,
    stackIdx: Int,
    bindings: List[(Name, Exp)],
    body: Exp
): Compile[List[Instruction]] = bindings match
  case Nil => compileExp(env, stackIdx, body)
  case (x, e) :: bs =>
    for {
      lCode <- compileExp(env, stackIdx, e)
      restCode <- compileLet(
        env + (x -> Binding.StackPos(stackIdx)),
        stackIdx - wordSize,
        bs,
        body
      )
    } yield lCode ++ List(s"    movq %rax, ${stackIdx}(%rsp)") ++ restCode

def compileApp(
    env: Env,
    stackIdx: Int,
    label: Label,
    args: List[Exp]
): Compile[List[Instruction]] = {

  def compileArgs(stackIdx: Int, args: List[Exp]): Compile[List[Instruction]] =
    args match
      case head :: next =>
        for {
          code <- compileExp(env, stackIdx, head)
          rest <- compileArgs(stackIdx - wordSize, next)
        } yield code ++ List(s"    movq %rax, ${stackIdx}(%rsp)") ++ rest
      case Nil => pure(List.empty)

  for {

    argsCode <- compileArgs(stackIdx - wordSize, args)

  } yield argsCode ++ List(
    s"    addq $$${stackIdx + wordSize}, %rsp", // Adjust stack pointer to be above local variables
    s"    callq $label",
    s"    addq $$${-(stackIdx + wordSize)}, %rsp" // Readjust
  )
}

def compileExp(env: Env, stackIdx: Int, e: Exp): Compile[List[Instruction]] = e match
  case Exp.Var(x)        => EitherT.fromEither(compileVar(env, x))
  case Exp.C(c)          => EitherT.pure(List(s"    movq ${constToImm(c)}, %rax"))
  case ifE: Exp.If       => compileIf(env, stackIdx, ifE)
  case Exp.UnOp(p, e)    => compileExp(env, stackIdx, e).map(_ ++ compileUnPrim(p))
  case binop: Exp.BinOp  => compileBinOp(env, stackIdx, binop)
  case Exp.Let(xs, body) => compileLet(env, stackIdx, xs, body)
  case Exp.App(lvar, args) =>
    (env.get(lvar), args) match {
      case (Some(Binding.ProgramLabel(l, arity)), args) if args.length == arity =>
        compileApp(env, stackIdx, l, args)
      case (Some(Binding.Toplevel(Builtin.Zeroary(e))), Nil) =>
        compileExp(env, stackIdx, e)
      case (Some(Binding.Toplevel(Builtin.Unary(f))), List(e)) =>
        compileExp(env, stackIdx, f(e))
      case (Some(Binding.Toplevel(Builtin.Binary(f))), List(e1, e2)) =>
        compileExp(env, stackIdx, f(e1, e2))
      case (_, _) => error("Unbound function")
    }
