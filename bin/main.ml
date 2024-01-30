open Core
open Lib

let get_all_tokens lex =
  let open Lexer in
  let rec aux lex token_list =
    let lex', token = next_token lex in
    print_endline (Token.as_string token);
    match token with
    | EOF as token -> List.rev (token :: token_list)
    | token -> aux lex' (token :: token_list)
  in
  aux lex []
;;

let _ =
  let open Lib.Lexer in
  let lex = new_lexer {|test "test" 123 + = != == &|} in
  let tokens = get_all_tokens lex in
  List.iter tokens ~f:(fun t -> print_endline (Token.as_string t))
;;
