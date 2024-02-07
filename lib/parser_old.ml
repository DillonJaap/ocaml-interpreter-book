open Core
open Result.Let_syntax

type t = {
  lexer : Lexer.t;
  errors : string list;
  current : Token.t option;
  peek : Token.t option;
}

let ( let* ) res f = Result.bind res ~f

let advance parser =
  let lexer, token = Lexer.next_token parser.lexer in
  let peek = match parser.current with Some EOF -> None | _ -> Some token in
  { parser with current = parser.peek; lexer; peek }

let skip parser ~token =
  match parser.peek with
  | Some peek when Token.compare token peek = 0 -> advance parser
  | _ -> parser

let expect_and_advance parser ~token =
  match parser.peek with
  | Some peek when Token.compare token peek = 0 -> Ok (advance parser)
  | _ -> Error "this is a failure TODO"

let new_parser lexer =
  advance @@ advance { lexer; errors = []; current = None; peek = None }

let parser_with_error parser error =
  let errors = List.append parser.errors [ error ] in
  advance { parser with errors }

let rec parse parser =
  let rec aux parser (statements : Ast.statement list) =
    let res =
      match parser.current with
      | Some Let -> parse_let_statement (advance parser)
      | Some Return -> parse_return_statement parser
      | Some EOF -> Error "end of file TODO update"
      | Some _ -> Error "TODO other token"
      | None -> Error "It is the end; TODO update"
    in
    match res with
    | Ok (p, stmt) -> aux (advance p) (stmt :: statements)
    | Error _ -> statements
  in
  aux parser [] |> List.rev

and parse_statement parser =
  match parser.current with
  | Some Let -> parse_let_statement (advance parser)
  | Return -> parse_return_statement parser
  | Some EOF -> Error "end of file TODO update"
  | Some _ -> Error "TODO other token"
  | None -> Error "It is the end; TODO update"

and parse_expression parser = (parser, Ast.Identifier "TODO temp")

and parse_identifier (parser : t) : (t * Ast.identifier, string) result =
  match parser.current with
  | Some (Identifier i) -> Ok (parser, i)
  | _ -> Error "expected identifier"

and parse_let_statement parser =
  let%bind parser, name2 = parse_identifier parser in
  let%bind parser = expect_and_advance parser ~token:Token.Assign in
  let parser, value = parse_expression parser in
  let parser = skip parser ~token:Token.Semicolon in
  Ok (parser, Ast.Let { name = name2; value })

and parse_return_statement parser =
  let parser, value = parse_expression parser in
  let parser = skip parser ~token:Token.Semicolon in
  Ok (parser, Ast.Return { value })
