open Core

let get_all_tokens lex =
  let open Monkey_lexer in
  let rec aux lex token_list =
    match next_token lex with
    | Some lex', token -> aux lex' (token :: token_list)
    | None, token -> List.rev (token :: token_list)
  in
  aux lex []
;;

let _ =
  let open Monkey_lexer in
  match new_lexer {|"test" + |} with
  | None -> ()
  | Some lex ->
    let tokens = get_all_tokens lex in
    List.iter tokens ~f:(fun t -> print_endline (Tokens.as_string t))
;;
