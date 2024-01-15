open Core

type lexer =
  { pos : int
  ; read_pos : int
  ; input : string
  ; ch : char
  }

let new_lexer input = { pos = 0; read_pos = 0; input; ch = ' ' }

let read_char lex =
  let ch =
    match lex.read_pos with
    | r when r >= String.length lex.input -> Char.of_int_exn 0
    | _ -> String.get lex.input lex.read_pos
  in
  { lex with ch; read_pos = lex.read_pos + 1; pos = lex.read_pos }
;;

let _ =
  let lex = new_lexer "test" in
  let new_lex = read_char lex in
  new_lex
;;
