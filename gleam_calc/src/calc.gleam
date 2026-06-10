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
      <> "[()\\+\\-*/]|"
      // not whitespace or parens or operations
      // (so numbers or funcions)
      <> "[^\\s()\\+\\-*/]+"
      <> ")",
    )
  let matches = regexp.scan(regex, expr)
  list.map(matches, fn(match) {
    let assert Match(_, [Some(str)]) = match
    case str {
      "(" -> LeftParen
      ")" -> RightParen
      "+" | "-" | "*" | "/" -> Operator(str)
      _ -> OtherToken(str)
    }
  })
}

fn parse(expr: List(Token)) -> ASTNode {
  option.unwrap(parse_(expr, None), Number(0.0))
}

fn add_to_ast(ast: Option(ASTNode), node: ASTNode) -> ASTNode {
  use ast <- unwrap_or_return(ast, node)
  todo as "add to ast"
  // case ast {
  //
  // }
}

// only nubers or functions or idk
fn expr_from_tokens(tokens: List(Token)) -> #(ASTNode, List(Token)) {
  case tokens {
    // ["(", ..rest] -> {
    //   parens(node_from_expr(rest))
    // }
    [Operator("+"), OtherToken(number), ..rest]
    | [Operator("-"), OtherToken(number), ..rest]
    | [OtherToken(number), ..rest] -> {
      let fl = float.parse(number)
      let fl =
        result.lazy_or(fl, fn() { result.map(int.parse(number), int.to_float) })
      let element = case fl {
        Ok(fl) -> Number(fl)
        _ -> todo as "literals/function"
      }

      case tokens {
        [Operator("-"), ..] -> #(Negate(element), rest)
        _ -> #(element, rest)
      }
    }
    _ -> todo as "unknown node"
  }
}

fn is_preceding(root: String, new: String) {
  let get_prio = fn(node: String) -> Int {
    case node {
      "*" | "/" -> 60
      _ -> 50
    }
  }

  get_prio(root) < get_prio(new)
}

fn parse_(tokens: List(Token), ast: Option(ASTNode)) -> Option(ASTNode) {
  use <- bool.guard(list.is_empty(tokens), ast)
  case tokens, ast {
    _, None -> {
      let #(node, rest) = expr_from_tokens(tokens)
      parse_(rest, Some(node))
    }
    // new operation after other operation
    [Operator(new_op), ..rest], Some(BinaryOp(root_op, root_l, root_r)) -> {
      let #(new_right, rest) = expr_from_tokens(rest)

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
      parse_(rest, Some(ast))
    }
    // new operation
    [Operator(op), ..rest], Some(ast) -> {
      let #(right, rest) = expr_from_tokens(rest)
      let ast = BinaryOp(op, ast, right)
      parse_(rest, Some(ast))
    }
    _, Some(ast) -> Some(ast)
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
        _ -> todo as "idk how to eval this op"
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

fn unwrap_or_return(opt: Option(a), node: a, callback: fn(a) -> a) -> a {
  case opt {
    None -> node
    Some(value) -> callback(value)
  }
}
