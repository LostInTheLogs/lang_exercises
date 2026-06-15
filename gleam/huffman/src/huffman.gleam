import gleam/dict.{type Dict}
import gleam/io
import gleam/list
import gleam/option
import gleam/result
import gleam/string
import input.{input}

pub fn main() -> Nil {
  // let in = result.unwrap(input("> "), "q")
  let in = "abcccaab"

  case in {
    "q" | "exit" | "quit" -> Nil
    "" -> main()
    in -> {
      in
      |> count_frequency()
      |> echo as "freq"
      |> freq_to_coding()
      |> echo as "coding"

      Nil
      // |> io.println
      // repl()
    }
  }
}

fn count_frequency(str: String) -> Dict(String, Int) {
  let letters = str |> string.to_graphemes()
  let uniq = letters |> list.unique()

  let count_letter = fn(el) { list.count(letters, fn(lel) { lel == el }) }

  let kv = list.map(uniq, fn(el) { #(el, count_letter(el)) })
  dict.from_list(kv)
}

fn freq_to_coding(coding: Dict(String, Int)) -> Dict(String, String) {
  todo
}
