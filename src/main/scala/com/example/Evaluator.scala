package com.example

import cats.implicits.*
import cats.effect.std.Console
import cats.Monad
import cats.MonadThrow

trait Evaluator[F[_]] {
  def exec(program: List[Expression]): F[Unit]
}

object Evaluator {

  import Expression.*

  def sequential[F[_]: Console: MonadThrow](errorPrinter: ErrorPrinter[F]): Evaluator[F] = new {

    type Store = Map[String, Expression]

    def execute(expr: Expression, context: Store): F[(Expression, Store)] = expr match {

      case l: (StringLiteral | IntLiteral | UnitValue) => (l, context).pure[F]

      case Binding(name, value, _, _)        =>
        execute(value, context) map { case (evaluatedValue, _) =>
          val updatedContext = context + (name.name -> evaluatedValue)
          (evaluatedValue, updatedContext)
        }
      case d: Def                            => (d -> context).pure[F]
      case Apply(Ref("print", _), args, loc) =>
        args match {
          case expr :: _ =>
            execute(expr, context) flatMap {
              case (StringLiteral(value), context) => Console[F].println(value).as((UnitValue(), context))
              case _                               => errorPrinter.printError(loc, "print expects string argument").as((UnitValue(), context))
            }
          case Nil       => Console[F].errorln("print expects exactly one argument").as((UnitValue(), context))
        }
      case Apply(funName, args, _)           =>
        context.get(funName.name) match {
          case Some(Def(wantedArgs, body, _)) =>
            for {
              evaluatedArgs <- args.traverse(arg => execute(arg, context))
              tmpContext     = context ++ (wantedArgs zip evaluatedArgs).map { case ((argName, _), (argValue, _)) =>
                                 argName.name -> argValue
                               }
              result        <- evalBodyExps(body, tmpContext)
            } yield (result._1, context)

          case Some(_) | None => Exception(s"Function not defined or incorrect type for $expr").raiseError
        }
      case Ref(name, _)                      =>
        context.get(name) match {
          case Some(value) => (value, context).pure[F]
          case None        => Exception("Undefined reference: " + name).raiseError
        }
    }

    private def evalBodyExps(body: List[Expression], context: Store): F[(Expression, Store)] = {
      body match {
        case Nil      => (UnitValue(), context).pure[F]
        case h :: Nil => execute(h, context)
        case h :: tl  => execute(h, context).flatMap { case (_, store) => evalBodyExps(tl, store) }
      }
    }

    def exec(program: List[Expression]): F[Unit] = {
      program
        .foldLeft[F[(List[Expression], Map[String, Expression])]](
          Monad[F].pure((List.empty[Expression], Map.empty[String, Expression]))
        ) { (acc, expr) =>
          acc.flatMap { case (_, context) =>
            execute(expr, context).map { case (result, updatedContext) =>
              (result :: Nil, updatedContext)
            }
          }
        }
        .map(_ => ())
    }
  }
}
