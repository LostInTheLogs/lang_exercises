import gleam/bool
import gleam/float
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regexp.{Match}
import gleam/result
import input.{input}

type Token {
  Number(value: Float)
  Operator(op: String)
  Literal(value: String)
  LeftParen
  RightParen
}

type ASTNode {
  ASTNode
  Parens(inside: ASTNode)
  BinaryOperation(op: String, left: ASTNode, right: Option(ASTNode))
}

pub fn main() -> Nil {
  io.println("Calculator")
  io.println("Type 'q' or 'exit' or 'quit' to quit")
  let _ = repl()
  io.println("bye")
}

fn lex(expr: String) -> List(Token) {
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
    let assert Match(_, [Some(str)]) = match
    case str {
      "(" -> LeftParen
      ")" -> RightParen
      "+" | "-" | "*" | "/" -> Operator(str)
      _ -> {
        let fl = float.parse(str)
        case fl {
          Ok(fl) -> Number(fl)
          Error(_) -> Literal(str)
        }
      }
    }
  })
}

fn parse(expr: List(Token)) -> ASTNode {
  let assert #(result, []) = parse_(expr, None)
  result
}

fn add_to_ast(ast: Option(ASTNode), node: ASTNode) -> ASTNode {
  use ast <- unwrap_or_return(ast, node)
  case ast {
  }
}

fn parse_(expr: List(Token), head: Option(ASTNode)) -> #(ASTNode, List(Token)) {
  case expr {
    [RightParen, ..rest] -> 
    [LeftParen, ..rest] -> {
      let #(insides, rest) = parse_(rest, option.None)
      let parens = Parens(insides)
      parse_(rest, Some(add_to_ast(head, parens)))
    }
    _ -> #(ASTNode, [])
  }
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

fn unwrap_or_return(opt: Option(a), node: a, callback: fn(a) -> a) -> a {
  case opt {
    None -> node
    Some(value) -> callback(value)
  }
}
