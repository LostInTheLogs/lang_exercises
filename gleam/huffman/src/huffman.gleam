import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import input.{input}

pub fn main() -> Nil {
  // let in = result.unwrap(input("> "), "q")
  let in =
    "aaaaabbbbbbbbbccccccccccccdddddddddddddeeeeeeeeeeeeeeeefffffffffffffffffffffffffffffffffffffffffffff"

  case in {
    "q" | "exit" | "quit" -> Nil
    "" -> main()
    in -> {
      in
      |> count_frequency()
      |> echo as "freq"
      |> freq_to_tree()
      |> echo as "coding"
      |> tree_to_dict(dict.new(), "")
      |> echo as "map"
      Nil
      // |> io.println
      // repl()
    }
  }
}

fn count_frequency(str: String) -> List(#(String, Int)) {
  let letters = str |> string.to_graphemes()
  let uniq = letters |> list.unique()

  let count_letter = fn(el) { list.count(letters, fn(lel) { lel == el }) }

  list.map(uniq, fn(el) { #(el, count_letter(el)) })
  |> list.sort(fn(a, b) { int.compare(a.1, b.1) })
}

type Node {
  Node(freq: Int, left: Node, right: Node)
  Leaf(freq: Int, char: String)
}

fn freq_to_tree(freq: List(#(String, Int))) -> Node {
  let nodes = list.map(freq, fn(el) { Leaf(el.1, el.0) })
  nodes_to_tree(nodes)
}

fn nodes_to_tree(freq: List(Node)) -> Node {
  case freq {
    [a, b, ..rest] -> {
      let new_node = Node(a.freq + b.freq, a, b)
      let #(lower_freq, higher_freq) =
        list.split_while(rest, fn(el) { el.freq < new_node.freq })
      let new_list = list.append(lower_freq, [new_node, ..higher_freq])
      nodes_to_tree(new_list)
    }
    [a] -> a
    [] -> panic
  }
}

fn tree_to_dict(
  root: Node,
  d: Dict(String, String),
  prefix: String,
) -> Dict(String, String) {
  case root {
    Leaf(_, char) -> {
      let prefix = case prefix {
        "" -> "0"
        _ -> prefix
      }
      dict.insert(d, char, prefix)
    }
    Node(_, left, right) -> {
      let d = tree_to_dict(left, d, prefix <> "0")
      tree_to_dict(right, d, prefix <> "1")
    }
  }
}
