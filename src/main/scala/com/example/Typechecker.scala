package com.example

import cats.Monad
import cats.implicits.*
import com.example.Expression.*
import com.example.Type.*

trait Typechecker[F[_]] {
  def check(expressions: List[Expression]): F[(Boolean, Type)]
}

object Typechecker {
  def default[F[_]: Monad](errorPrinter: ErrorPrinter[F]): Typechecker[F] = new {

    override def check(expressions: List[Expression]): F[(Boolean, Type)] = {
      expressions.foldLeft(Monad[F].pure((true, TUnit))) { case (acc, expr) =>
        acc.flatMap { case (noErrorsSoFar, lastType) =>
          checkExpr(expr, Map.empty).map { case (isSuccess, tpe) =>
            (noErrorsSoFar && isSuccess, tpe)
          }
        }
      }
    }

    def checkExpr(expression: Expression, context: Map[String, Type]): F[(Boolean, Type)] =
      expression match {
        case StringLiteral(_) => (true, TString).pure[F]
        case IntLiteral(_)    => (true, TInt).pure[F]
        case UnitValue()      => (true, TUnit).pure[F]

        case Binding(name, value, tpe, loc) =>
          checkExpr(value, context).flatMap { case (valueSuccess, valueType) =>
            if valueType == tpe then (valueSuccess, tpe).pure[F]
            else errorPrinter.printError(loc, show"Type mismatch for ${name.name}: expected $tpe, got $valueType").as((false, Type.TAny))

          }

        case Def(args, body, _) =>
          val argTypes   = args map { case (ref, tpe) => (ref.name, tpe) }
          val newContext = context ++ argTypes

          body
            .foldLeft(Monad[F].pure((true, TUnit))) { case (acc, expr) =>
              acc.flatMap { case (noErrorsSoFar, lastType) =>
                checkExpr(expr, newContext).map { case (isSuccess, tpe) => (noErrorsSoFar && isSuccess, tpe) }
              }
            }
            .map { case (success, lastBodyType) => (success, TFun(argTypes.headOption.map(_._2).getOrElse(TUnit), lastBodyType)) }

        case Apply(Ref("print", _), args, loc) =>
          args match {
            case expr :: Nil =>
              checkExpr(expr, context).flatMap {
                case (exprSuccess, TString)  => (exprSuccess, TString).pure[F]
                case (exprSuccess, exprType) => errorPrinter.printError(loc, show"print expects a String argument, got $exprType").as(false, TAny)
              }
            case all         => errorPrinter.printError(loc, s"print expects exactly one argument, got ${all.length}").as(false, TAny)
          }

        case Apply(fun, args, loc) =>
          checkExpr(fun, context).flatMap {
            case (funSuccess, TFun(in, out)) if args.length == 1 =>
              checkExpr(args.head, context).flatMap {
                case (argSuccess, argType) if argType == in => Monad[F].pure((funSuccess && argSuccess, out))
                case (argSuccess, argType)                  => errorPrinter.printError(loc, s"Expected argument of type $in, got $argType").as(false, TAny)
              }

            case (funSuccess, TFun(_, _)) => errorPrinter.printError(loc, "Currently only single argument functions supported").as(false, TAny)
            case (funSuccess, _)          => errorPrinter.printError(loc, s"Expected function type").as(false, TAny)
          }

        case Ref(name, loc) =>
          context.get(name).fold(errorPrinter.printError(loc, s"Undefined reference: $name").as(false, TAny))(tpe => (true, tpe).pure[F])
      }
  }
}
