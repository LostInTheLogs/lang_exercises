import gleam/bool
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/regexp
import gleam/result
import gleam/string
import input.{input}

// type Token {
//   Number(value: Float)
//   PlusSign
//   MinusSign
//   LeftParen
//   RightParen
//   UnknownToken(str: String)
// }

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

fn lex(expr: String) -> List(String) {
  let assert Ok(regex) =
    regexp.from_string(
      // ignore whitespace
      "\\s*"
      // match:
      <> "("
      // parens, operations OR
      <> "[()\\+\\-*/]|"
      // not whitespace or parens or operations
      // (so numbers or funcions)
      <> "[^\\s()\\+\\-*/]"
      <> ")",
    )
  let matches = regexp.scan(regex, expr)
  list.map(matches, fn(match) {
    let regexp.Match(str, _) = match
    str
  })
}

fn parse(expr: List(String)) -> ASTNode {
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
