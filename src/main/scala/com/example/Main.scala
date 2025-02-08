package com.example

import cats.effect.IOApp
import cats.effect.IO
import java.nio.file.Path
import cats.effect.ExitCode
import scala.util.Try
import java.nio.file.Files
import java.nio.charset.Charset
import cats.syntax.foldable.*
import cats.syntax.show.*
import cats.effect.std.Console

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    val result = for {
      firstArg         <- IO.fromOption(args.headOption)(RuntimeException(s"path is required, got `$args`"))
      path             <- IO.fromTry(Try(Path.of(firstArg)))
      _                <- IO.println(s"Reading from $path")
      contents         <- IO.blocking(String(Files.readString(path, Charset.forName("UTF-8"))))
      _                <- IO.println(s"Contents is $contents")
      parser            = Parser.live[IO]
      parsed           <- parser.parse(contents)
      _                <- IO.whenA(parsed.isLeft)(IO.println(parsed))
      source           <- IO.fromEither(parsed.left.map(e => Exception(s"Parsing Error: $e")))
      _                <- source.traverse_(expression => IO.println(expression.show))
      given Console[IO] = Console.make[IO]
      _                <- Evaluator.sequential.exec(source)
    } yield ()
    result >> IO(ExitCode.Success)
}
