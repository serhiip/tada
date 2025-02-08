package com.example

import cats.syntax.applicative.*
import cats.Applicative

trait Parser[F[_], E] {
  def parse(source: String): F[Either[E, List[Expression]]]
}

object Parser {

  import cats.parse.{Parser as P}
  import cats.parse.Parser.*
  import cats.parse.Rfc5234.{alpha, digit, wsp, char as asciiChar, dquote, crlf}
  import cats.parse.Parser0

  def live[F[_]: Applicative]: Parser[F, P.Error] = new {

    val parsing = P.recursive[Expression] { recurse =>

      val whitespace: P[Unit]                  = charIn(" \t\r\n").void
      val whitespaces0: Parser0[Unit]          = whitespace.rep0.void
      val listSep: P[Unit]                     = char(',').soft.surroundedBy(whitespaces0).void
      def rep[A](pa: P[A]): Parser0[List[A]]   = pa.repSep0(listSep).surroundedBy(whitespaces0)
      def repNl[A](pa: P[A]): Parser0[List[A]] = pa.repSep0(crlf.void).surroundedBy(whitespaces0)

      val stringLiteral: P[Expression.StringLiteral] = {
        asciiChar.repUntil0(dquote).with1.surroundedBy(dquote).map(string => Expression.StringLiteral(String(string.iterator.toArray)))
      }

      val intLiteral: P[Expression.IntLiteral] = digit.rep.map(digits =>
        Expression.IntLiteral(
          digits.reverse.zipWithIndex.map((asciiChar, power) => asciiChar.asDigit * Math.pow(10, power).toInt).reduce
        )
      )

      val literal: P[Expression] = stringLiteral | intLiteral
      val ref: P[Expression.Ref] = (alpha | digit).rep.map(name => Expression.Ref(String(name.iterator.toArray)))

      val application = {
        val parse = ref.soft ~ rep(literal | recurse).with1.between(char('('), char(')'))
        parse.map { case (ref, args) => Expression.Apply(ref, args) }
      }

      val definition: P[Expression.Def] = {
        val argList = rep(ref).between(char('('), char(')'))
        val body    = repNl(literal | recurse).between(char('{').surroundedBy(whitespaces0), char('}').surroundedBy(whitespaces0))
        val parsed  = (argList.with1 <* wsp.rep <* string("=>")) ~ (wsp.rep *> body)
        parsed.map((args, body) => Expression.Def(args.map(_ -> Type.TAny), body))
      }

      val binding: P[Expression.Binding] = {
        val lhs    = string("var") *> wsp.rep *> ref
        val rhs    = (literal | recurse).surroundedBy(whitespaces0)
        val parsed = (lhs <* wsp.rep <* char('=') <* wsp.rep) ~ rhs
        parsed.map((name, value) => Expression.Binding(name, value, Type.TAny))
      }

      P.oneOf(binding :: definition :: application :: ref :: Nil)

    }

    override def parse(source: String): F[Either[P.Error, List[Expression]]] = {
      def recur(remainder: String, acc: Either[P.Error, List[Expression]]): Either[P.Error, List[Expression]] = {
        acc flatMap { expressions =>
          if remainder.isBlank() then Right(expressions.reverse)
          else {
            parsing.parse(remainder).flatMap((remainder, result) => recur(remainder, Right(result :: expressions)))
          }
        }

      }
      recur(source, Right(List.empty[Expression])).pure[F]
    }
  }
}
