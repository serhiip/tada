# TaDa Language Interpreter

This project implements a simple interpreter for a custom language called **TaDa**. It demonstrates a basic language processing pipeline, including parsing, evaluation, and basic typechecking concepts.

## Purpose

The goal of this project is to showcase a simplified, yet functional, language interpreter. It illustrates how to:

*   Define a language grammar using a parser combinator library (`cats-parse`).
*   Represent the language's abstract syntax tree (AST) using Scala's `enum`.
*   Evaluate the AST to execute the program.
*   Simulate a limited form of "typechecking" through the evaluation process.

## The TaDa Language

TaDa is a small, dynamically-typed language with the following features:

*   **Literals:** String and integer literals.
*   **Variables:** Variables can be declared using `var`.
*   **Functions:** Functions can be defined using the `(arg1, arg2) => { body }` syntax.
*   **Function Application:** Functions can be called using the `functionName(arg1, arg2)` syntax.
*   **References:** Variables and functions can be referenced by their names.
*    No loops.

### Examples
There are two example files `example.tada` and `example2.tada`.
* `example.tada` defines function `a` and applies it to the arguments `42` and `"aaaaaaa"`.
```tada
var a = (n1, n2) => {
  n(42, "aaaaaaa")
}
```
* `example2.tada` defines a single variable `b` initialized to string value `"nth"`.
```tada
var b = "nth"
```

## Project Structure

The project is organized into the following directory structure:

```
.
├── src
│   └── main
│       └── scala
│           └── com
│               └── example
│                   ├── Evaluator.scala   // Evaluates the AST.
│                   ├── Expression.scala  // Defines the AST (Expression enum).
│                   ├── Main.scala        // Main application entry point.
│                   └── parser
│                       └── Parsing.scala   // Parser definition.
├── build.sbt             // sbt build configuration.
└── README.md               // This file.
```

*   **`Expression.scala`:** Defines the `Expression` enum, which represents the different types of nodes in the abstract syntax tree (AST). This includes literals, variable bindings, function definitions, function applications, and references. It has also instance of `cats.Show` defined for debugging purposes.
*   **`Parsing.scala`:** Contains the parser, implemented using `cats-parse`. It defines the grammar of the TaDa language and how to convert a string input into an `Expression` AST.
*   **`Evaluator.scala`:** Implements the `Evaluator` trait, specifically the `sequential` evaluator. This component takes a list of `Expression`s (the AST) and executes them, maintaining a "store" (a simple `Map`) to simulate variable bindings and function definitions.
*   **`Main.scala`:** The main entry point of the application. It reads a TaDa program from a file (specified as a command-line argument), parses it using `Parsing.parsing`, prints parsed program and then runs it through the evaluator.
* **`example.tada` and `example2.tada`**: example programs.

## Building and Running

### Prerequisites

*   Java Development Kit (JDK) 8 or later.
*   [sbt](https://www.scala-sbt.org/) (Scala Build Tool).

### Build

Use sbt to build the project:

```bash
sbt compile
```

### Run

The application takes a file path as a command-line argument, where file contains tada source code:

```bash
sbt "run <path_to_tada_file>"
```

For instance, to execute code inside `example.tada` file in the root of this project:

```bash
sbt "run example.tada"
```
Or for `example2.tada`:
```bash
sbt "run example2.tada"
```

### Test
There are no defined tests
```bash
sbt test
```

## sbt-tpolecat

This template uses the `sbt-tpolecat` sbt plugin to set Scala compiler options to recommended defaults. If you want to change these defaults or find out about the different modes the plugin can operate in you can find out [here](https://github.com/typelevel/sbt-tpolecat/).
