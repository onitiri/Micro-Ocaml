# Micro-Ocaml
A subset of the Ocaml programming language
# Project Name

## Overview

This project is an implementation of a lexer, parser, and evaluation system that provides a subset of OCaml the OCaml language using OCaml. The project consists of multiple components including lexical analysis, parsing, and utility functions.

## Features

- **Lexer**: Tokenizes the input source code.
- **Parser**: Parses the tokens and constructs an abstract syntax tree (AST).
- **Evaluator**: Evaluates expressions from the AST.
- **Utilities**: Provides helper functions for handling parsing and evaluation.

## Project Structure

```
├── eval.ml       # Implementation of the evaluation logic
├── lexer.ml      # Lexical analysis of input expressions
├── parser.mli    # Interface for the parser
├── utils.ml      # Utility functions for parsing and evaluation
```

## Installation

Ensure you have OCaml and Dune installed.

```sh
opam install dune
```

## Build and Run

To compile the project, run:

```sh
dune build
```

To execute the program:

```sh
dune exec ./main.exe
```

## Usage

Provide an input file with expressions to evaluate, and the lexer and parser will process them accordingly. Alternatively, you can run utop and issue commands from there.

