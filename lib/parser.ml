open! Core

let ( let* ) res f = Result.bind res ~f

type t = {
  lexer : Lexer.t;
  errors : string list;
  current : Token.t;
  peek : Token.t;
}

let advance parser : t =
  let lexer, peek = Lexer.next_token parser.lexer in
  { parser with current = parser.peek; lexer; peek }

let skip parser ~token =
  match Token.compare token parser.peek with 0 -> advance parser | _ -> parser

let advance_if_match parser ~token =
  match Token.compare token parser.peek with
  | 0 -> Ok (advance parser)
  | _ -> Error "this is a failure TODO"

let new_parser lexer =
  advance { lexer; errors = []; current = Token.Illegal; peek = Token.Illegal }

let parser_with_error parser error =
  let errors = List.append parser.errors [ error ] in
  advance { parser with errors }

let rec parse_program parser =
  let open Ast in
  let rec aux parser (statements : statement list) =
    let parser = advance parser in
    let res =
      match parser.current with
      | Let -> parse_let_statement parser
      | Return -> parse_return_statement parser
      | EOF -> Error "end of file TODO update"
      | _ -> Error "end of file TODO update"
    in
    match res with
    | Ok (p, stmt) -> aux p (stmt :: statements)
    | Error _ -> statements
  in
  aux parser [] |> List.rev

and parse_let_statement (parser : t) : (t * Ast.statement, string) result =
  let* parser, name = parse_identifier parser in
  let* parser = advance_if_match parser ~token:Token.Assign in
  let parser, value = parse_expression parser in
  let parser = skip parser ~token:Token.Semicolon in
  Ok (parser, Ast.Let { name; value })

and parse_return_statement (parser : t) : (t * Ast.statement, string) result =
  let parser, value = parse_expression parser in
  let parser = skip parser ~token:Token.Semicolon in
  Ok (parser, Ast.Return value)

and parse_expression (parser : t) = (parser, Ast.Identifier "TODO temp")

and parse_identifier (parser : t) : (t * Ast.expression, string) result =
  match parser.current with
  | Identifier i -> Ok (advance parser, Ast.Identifier i)
  | _ -> Error "expected identifier"
