package com.example

import cats.effect.std.Console
import cats.syntax.all.*

import java.nio.file.Path

trait ErrorPrinter[F[_]] {
  def printError(forLocation: Info, errorDescription: String): F[Unit]
}

object ErrorPrinter {

  def runtimeErrors[F[_]: Console](pathToSource: Path, sourceContents: String): ErrorPrinter[F] = new {

    override def printError(forLocation: Info, errorDescription: String): F[Unit] = {
      val lines     = sourceContents.split("\n")
      val contextSize = 3
      val startLine = (forLocation.startsAt.line - contextSize).max(0)
      val endLine   = (forLocation.endsBefore.line + contextSize).min(lines.length)

      val contextLines = lines.slice(startLine, endLine).zipWithIndex.map { case (line, index) =>
        val lineNumber      = startLine + index + 1
        val isErrorLine     = lineNumber == forLocation.startsAt.line + 1
        val highlightStart  = forLocation.startsAt.column + lineNumber.toString.length
        val highlightEnd    = forLocation.endsBefore.column + lineNumber.toString.length
        val highlightPrefix = " " * highlightStart
        val highlight       = "^" * (highlightEnd - highlightStart).max(1)

        if (isErrorLine) {
          s"$lineNumber | $line\n ${" " * lineNumber.toString.length}|$highlightPrefix$highlight"
        } else {
          s"$lineNumber | $line"
        }
      }

      val output =
        s"""
           .Error in ${pathToSource}:${forLocation.startsAt.line}:${forLocation.startsAt.column}:
           .
           .${contextLines.mkString("\n")}
           .
           .$errorDescription
           """.stripMargin('.')

      Console[F].println(output)
    }
  }
}
