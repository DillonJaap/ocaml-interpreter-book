open! Core
open! Parser

let test_eval input =
  let node = Lexer.new_lexer input |> Parser.new_parser |> Parser.parse in
  let env = Environment.new_ () in
  Evaluator.eval node env

let%test_unit "test integer" =
  [%test_eq: Object.t] (test_eval "5") (Object.Integer 5)
