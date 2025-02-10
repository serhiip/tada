package com.example

import cats.Show
import cats.syntax.show.*

enum Type {
  case TAny, TString, TInt, TUnit
  case TFun(out: Type, in: Type*)
}

object Type {
  given Show[Type] = Show.show {
    case Type.TAny           => "Any"
    case Type.TString        => "String"
    case Type.TInt           => "Int"
    case Type.TUnit          => "Unit"
    case Type.TFun(out, in*) =>
      val delimiter = " -> "
      val inJoined  = in.map(_.show).mkString(delimiter)
      s"$inJoined$delimiter$out"
  }
}

case class Loc(offset: Int, line: Int, column: Int)
case class Info(startsAt: Loc, endsBefore: Loc)

enum Expression {
  // literal values
  case StringLiteral(value: String)
  case IntLiteral(value: Int)
  // assign names to values (e.g. `var a = ...`)
  case Binding(name: Ref, value: Expression, tpe: Type, location: Info)
  // define functions (e.g. `(a, b) => ...`)
  case Def(args: List[Ref], body: List[Expression], tpe: Type.TFun, location: Info)
  // apply functions to arguments to get a result (e.g. `foo("1", "bar")`)
  case Apply(it: Ref, args: List[Expression], location: Info)
  // reference existing binding
  case Ref(name: String, location: Info)
  case UnitValue()
}

object Expression {
  given Show[Expression] = Show.show { it => ExpressionPretty.print(it, 0) }
}

private object ExpressionPretty {
  def print(expression: Expression, level: Int): String = {
    val indent = "  " * level
    expression match {
      case Expression.StringLiteral(value)                =>
        s"""${indent}"${value}": String"""
      case Expression.IntLiteral(value)                   =>
        s"${indent}${value}: Int"
      case Expression.Binding(name, value, tpe, location) =>
        s"${indent}Binding(${print(name, 0)}, ${print(value, level + 1)}, ${tpe.show}, ${print(location)})"
      case Expression.Def(args, body, tpe, location)      =>
        val argsStr = args.map(print(_, 0)).mkString(", ")
        val bodyStr = body.map(print(_, level + 1)).mkString("\n")
        show"${indent}Def(${argsStr}, $tpe, ${print(location)}) {\n${bodyStr}\n${indent}}"
      case Expression.Apply(it, args, location)           =>
        val argsStr = args.map(print(_, level + 1)).mkString(",\n")
        s"${indent}Apply(${print(it, 0)}, ${print(location)},\n${argsStr})"
      case Expression.Ref(name, location)                 =>
        s"${indent}${name} ${print(location)}"
      case Expression.UnitValue()                         =>
        s"${indent}(): Unit"
    }
  }

  private def print(ref: Expression.Ref, level: Int): String = {
    val indent = "  " * level
    s"${indent}${ref.name} ${print(ref.location)}"
  }

  private def print(location: Info): String = {
    s"[${location.startsAt.line}:${location.startsAt.column}-${location.endsBefore.line}:${location.endsBefore.column}]"
  }
}
