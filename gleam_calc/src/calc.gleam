import gleam/bool
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regexp.{Match}
import gleam/result
import input.{input}

type Token {
  //   Number(value: Float)
  Operator(op: String)
  LeftParen
  RightParen
  OtherToken(str: String)
}

type ASTNode {
  Number(value: Float)
  Negate(inside: ASTNode)
  Parens(inside: ASTNode)
  BinaryOp(op: String, left: ASTNode, right: ASTNode)
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
      <> "[()\\+\\-*/^]|"
      // not whitespace or parens or operations
      // (so numbers or funcions)
      <> "[^\\s()\\+\\-*/^]+"
      <> ")",
    )
  let matches = regexp.scan(regex, expr)
  list.map(matches, fn(match) {
    let assert Match(_, [Some(str)]) = match
    case str {
      "(" -> LeftParen
      ")" -> RightParen
      "+" | "-" | "*" | "/" | "^" -> Operator(str)
      _ -> OtherToken(str)
    }
  })
}

fn parse_literals(tokens: List(Token)) -> #(ASTNode, List(Token)) {
  case tokens {
    [OtherToken("pi"), ..rest] -> #(Number(3.1415926535897932384), rest)
    [OtherToken(token), ..] -> panic as { "unknown literal: " <> token }
    _ -> panic as "unexpected arg in parse_literals"
  }
}

// only nubers or functions or idk
fn parse_expr(tokens: List(Token)) -> #(ASTNode, List(Token)) {
  case tokens {
    [LeftParen, ..] -> {
      let assert #(Some(parens), rest) = parse_with_rest(tokens, None)
        as "empty parens"
      #(parens, rest)
    }
    [Operator("+"), OtherToken(number), ..rest]
    | [Operator("-"), OtherToken(number), ..rest]
    | [OtherToken(number), ..rest] -> {
      let fl = float.parse(number)
      let fl =
        result.lazy_or(fl, fn() { result.map(int.parse(number), int.to_float) })
      let #(element, rest) = case fl {
        Ok(fl) -> #(Number(fl), rest)
        _ -> parse_literals([OtherToken(number), ..rest])
      }

      case tokens {
        [Operator("-"), ..] -> #(Negate(element), rest)
        _ -> #(element, rest)
      }
    }
    _ -> panic as "unknown node"
  }
}

fn is_preceding(root: String, new: String) {
  let get_prio = fn(node: String) -> Int {
    case node {
      "^" -> 70
      "*" | "/" -> 60
      _ -> 50
    }
  }

  get_prio(root) < get_prio(new)
}

fn parse(expr: List(Token)) -> ASTNode {
  let #(ast, _) = parse_with_rest(expr, None)
  option.unwrap(ast, Number(0.0))
}

fn parse_with_rest(
  tokens: List(Token),
  ast: Option(ASTNode),
) -> #(Option(ASTNode), List(Token)) {
  use <- bool.guard(list.is_empty(tokens), #(ast, []))
  case tokens, ast {
    [LeftParen, ..rest], _ -> {
      case parse_with_rest(rest, None) {
        #(Some(insides), rest) -> #(Some(Parens(insides)), rest)
        #(None, rest) -> #(None, rest)
      }
    }
    [RightParen, ..rest], _ -> #(ast, rest)
    _, None -> {
      let #(node, rest) = parse_expr(tokens)
      parse_with_rest(rest, Some(node))
    }
    // new operation after other operation
    [Operator(new_op), ..rest], Some(BinaryOp(root_op, root_l, root_r)) -> {
      let #(new_right, rest) = parse_expr(rest)

      let ast = case is_preceding(root_op, new_op) {
        True -> {
          let new_right = BinaryOp(new_op, root_r, new_right)
          BinaryOp(root_op, root_l, new_right)
        }
        False -> {
          let Some(ast) = ast
          BinaryOp(new_op, ast, new_right)
        }
      }
      parse_with_rest(rest, Some(ast))
    }
    // new operation
    [Operator(op), ..rest], Some(ast) -> {
      let #(right, rest) = parse_expr(rest)
      let ast = BinaryOp(op, ast, right)
      parse_with_rest(rest, Some(ast))
    }
    _, Some(ast) -> #(Some(ast), [])
  }
}

fn eval(ast: ASTNode) -> Float {
  case ast {
    Number(number) -> number
    Parens(inside) -> eval(inside)
    Negate(inside) -> float.negate(eval(inside))
    BinaryOp(op, left, right) -> {
      let left = eval(left)
      let right = eval(right)
      case op {
        "+" -> left +. right
        "-" -> left -. right
        "*" -> left *. right
        "/" -> left /. right
        "^" -> {
          let assert Ok(pow) = float.power(left, right)
          pow
        }
        _ -> panic as { "unknown operation: " <> op }
      }
    }
  }
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
      // |> echo as "lex"
      |> parse()
      |> echo as "parse output:"
      |> eval()
      |> to_string()
      |> io.println
      repl()
    }
  }
}
