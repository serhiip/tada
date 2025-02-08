package com.example

import cats.Show

enum Type {
  case TAny, TString, TInt, TUnit
  case TFun(in: Type, out: Type)
}

object Type {
  given Show[Type] = Show.show {
    case Type.TAny          => "Any"
    case Type.TString       => "String"
    case Type.TInt          => "Int"
    case Type.TUnit         => "Unit"
    case Type.TFun(in, out) => s"${in} -> ${out}"
  }
}

enum Expression {
  // literal values
  case StringLiteral(value: String)
  case IntLiteral(value: Int)
  // assign names to values (e.g. `var a = ...`)
  case Binding(name: Ref, value: Expression, tpe: Type)
  // define functions (e.g. `(a, b) => ...`)
  case Def(args: List[(Ref, Type)], body: List[Expression])
  // apply functions to arguments to get a result (e.g. `foo("1", "bar")`)
  case Apply(it: Ref, args: List[Expression])
  // reference existing binding
  case Ref(name: String)
  case UnitValue()
}

object Expression {
  given Show[Expression] = Show.show { it => ExpressionPretty.print(it, 0) }
}

private object ExpressionPretty {
  def print(expression: Expression, level: Int): String = {
    val indent = "  " * level
    expression match {
      case Expression.StringLiteral(value)      =>
        s"""${indent}"${value}": String"""
      case Expression.IntLiteral(value)         =>
        s"${indent}${value}: Int"
      case Expression.Binding(name, value, tpe) =>
        s"${indent}Binding(${print(name, 0)}, ${print(value, level + 1)}, ${tpe})"
      case Expression.Def(args, body)           =>
        val argsStr = args.map { case (ref, tpe) => s"${print(ref, 0)}: ${tpe}" }.mkString(", ")
        val bodyStr = body.map(print(_, level + 1)).mkString("\n")
        s"${indent}Def(${argsStr}) {\n${bodyStr}\n${indent}}"
      case Expression.Apply(it, args)           =>
        val argsStr = args.map(print(_, level + 1)).mkString(",\n")
        s"${indent}Apply(${print(it, 0)},\n${argsStr})"
      case Expression.Ref(name)                 =>
        s"${indent}${name}"
      case Expression.UnitValue()               =>
        s"${indent}(): Unit"
    }
  }

  def print(ref: Expression.Ref, level: Int): String = {
    val indent = "  " * level
    s"${indent}${ref.name}"
  }
}
