import gleam/int
import gleam/bool
import gleam/float
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import input.{input}

type Token {
  Number(value: Float)
  PlusSign
  MinusSign
  LeftParen
  RightParen
  UnknownToken(str: String)
}

type ASTNode {
  ASTNode
  BinaryOperation(op: String, left: ASTNode, right: ASTNode)
}

pub fn main() -> Nil {
  io.println("Calculator")
  io.println("Type 'q' or 'exit' or 'quit' to quit")
  let _ = repl()
  io.println("bye")
}

fn lex(expr: String) -> List(Token) {
  lex_tco(expr, [])
}

fn lex_tco(expr: String, acc: List(Token)) -> List(Token) {
  use <- bool.guard(string.is_empty(expr), list.reverse(acc))
  let #(token, rest) = case expr {
    "(" <> rest -> #(LeftParen, rest)
    ")" <> rest -> #(RightParen, rest)
    "+" <> rest -> #(PlusSign, rest)
    "-" <> rest -> #(MinusSign, rest)
    number -> {
      case float.parse(number) {
        Ok(fl) -> #(Number(fl), rest)
        Error(Nil) -> #(UnknownToken(number), rest)
      }
    }
  }
  lex_tco(rest, [token, ..acc])
}

fn parse(expr: List(Token)) -> ASTNode {
  ASTNode
}

fn eval(in: ASTNode) -> Float {
  1.0
}

fn to_string(ast: Float) -> String {
  float.to_string(ast)
}

fn repl() {
  let in = result.unwrap(input("> "), "q")

  case in {
    "q" | "exit" | "quit" -> Nil
    "" -> repl()
    in -> {
      in
      |> lex()
      |> echo as "lex"
      |> parse()
      |> echo as "parse"
      |> eval()
      |> echo as "eval"
      |> to_string()
      |> io.println
      repl()
    }
  }
}
