open! Core

let ( let* ) res f = Base.Result.bind res ~f

type t = {
  lexer : Lexer.t;
  errors : string list;
  current_token : Token.t;
  peek_token : Token.t;
}

let advance parser : t =
  let lexer, peek_token = Lexer.next_token parser.lexer in
  { parser with current_token = parser.peek_token; lexer; peek_token }

let peek_token parser =
  let lexer, peek_token = Lexer.next_token parser.lexer in
  ( peek_token,
    { parser with current_token = parser.peek_token; lexer; peek_token } )

let skip parser ~token =
  match Token.compare token parser.peek_token with
  | 0 -> advance parser
  | _ -> parser

let expect parser ~token =
  match Token.compare token parser.peek_token with
  | 0 -> Ok (advance parser)
  | _ -> Error "this is a failure TODO"

let peek_n_tokens parser n =
  let rec aux parser n tokens =
    match (parser.current_token, n) with
    | Token.EOF, _ -> (Token.EOF :: tokens, parser)
    | _, x when x < 1 -> ([], parser)
    | _, 1 -> (parser.peek_token :: tokens, parser)
    | _, _ -> aux (advance parser) (n - 1) (parser.peek_token :: tokens)
  in
  let l, p = aux parser n [] in
  (List.rev l, p)

let new_parser lexer =
  advance
    {
      lexer;
      errors = [];
      current_token = Token.Illegal;
      peek_token = Token.Illegal;
    }

let parser_with_error parser error =
  let errors = List.append parser.errors [ error ] in
  advance { parser with errors }

(* TODO do I need to have a parse identifier function here? *)
let rec parse_program parser =
  let open Ast in
  let aux parser (statements : statement list) =
    let _, token = Lexer.next_token parser.lexer in
    match token with
    | Let -> statements
    | Return -> statements
    | EOF -> statements
    | _ -> statements
  in
  aux parser [] |> List.rev

and parse_expression (parser : t) = (parser, Ast.Identifier "TODO temp")

and parse_identifier (parser : t) =
  match parser.peek_token with
  | Identifier i -> Ok (advance parser, Ast.Identifier i)
  | _ -> Error "expected identifier"

and parse_let_statement parser =
  (*  let* parser = expect parser ~token:Assign in *)
  let* parser, name = parse_identifier parser in
  let parser, value = parse_expression parser in
  Ok (parser, Ast.Let { name; value })
