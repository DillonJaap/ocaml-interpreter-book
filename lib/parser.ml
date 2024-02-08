open Core
open Result.Let_syntax

(*  let ( let* ) res f = Result.bind res ~f *)

type t = {
  lexer : Lexer.t;
  errors : string list;
  current : Token.t;
  peek : Token.t;
}
[@@deriving show]

type precidence =
  [ `Lowest
  | `Equals
  | `LessGreater
  | `Sum
  | `Product
  | `Prefix
  | `Call
  | `Index ]
[@@deriving show, ord]

let get_precidence (token : Token.t) =
  match token with
  | Equal | Not_Equal -> `Equals
  | Less_Than | Greater_Than -> `LessGreater
  | Plus | Minus -> `Sum
  | Slash | Asterisk -> `Product
  | Lparen -> `Call
  | Lbracket -> `Index
  | _ -> `Lowest

let advance parser =
  let lexer, peek = Lexer.next_token parser.lexer in
  { parser with current = parser.peek; lexer; peek }

let skip parser ~token =
  match Token.compare token parser.peek = 0 with
  | true -> advance parser
  | false -> parser

let expect_and_advance parser ~token =
  match Token.compare token parser.peek = 0 with
  | true -> Ok (advance parser)
  | false ->
      Error
        (sprintf "expected token: %s, got token %s" (Token.as_string token)
           (Token.as_string parser.peek))

let new_parser lexer =
  advance
  @@ advance
       { lexer; errors = []; current = Token.Illegal; peek = Token.Illegal }

let parser_with_error parser error =
  let errors = List.append parser.errors [ error ] in
  advance { parser with errors }

let rec parse parser =
  let rec aux parser (statements : Ast.statement list) =
    let res =
      match parser.current with
      | EOF -> None
      | _ -> Some (parse_statement parser)
    in
    match res with
    | Some (Ok (parser', stmt)) -> aux (advance parser') (stmt :: statements)
    | Some (Error e) ->
        print_endline e;
        aux (advance parser) statements
    | None -> statements
  in
  aux parser [] |> List.rev

and parse_statement parser =
  match parser.current with
  | Let -> parse_let_statement parser
  | Return -> parse_return_statement parser
  | _ -> parse_expression_statement parser

and parse_let_statement parser =
  let parser = advance parser in
  let%bind parser, name = parse_identifier parser in
  let%bind parser = expect_and_advance parser ~token:Token.Assign in
  let%bind parser, value = parse_expression (advance parser) `Lowest in
  let parser = skip parser ~token:Token.Semicolon in
  Ok (parser, Ast.Let { name; value })

and parse_return_statement parser =
  let parser = advance parser in
  let%bind parser, value = parse_expression parser `Lowest in
  let parser = skip parser ~token:Token.Semicolon in
  Ok (parser, Ast.Return { value })

and parse_expression_statement parser =
  let%bind parser, expr = parse_expression parser `Lowest in
  let parser = skip parser ~token:Token.Semicolon in
  Ok (parser, Ast.Expression_Statement { value = expr })

and parse_expression parser prec =
  let%bind parser, lexpr = parse_prefix_expression parser in
  let rec aux parser lexpr =
    let comp = compare_precidence (get_precidence parser.peek) prec in
    match (parser.peek, comp) with
    | Semicolon, _ -> Ok (parser, lexpr)
    | _, (0 | -1) -> Ok (parser, lexpr)
    | _, 1 ->
        let%bind parser, lexpr =
          parse_infix_expression (advance parser) lexpr
        in
        aux parser lexpr
    | _ -> Ok (parser, lexpr)
  in
  aux parser lexpr

and parse_infix_expression parser left =
  match parser.current with
  | Plus | Minus | Slash | Asterisk | Equal | Not_Equal | Less_Than
  | Greater_Than ->
      parse_default_infix_expression parser left
  | Lparen -> parse_call_expression parser left
  | Lbracket -> Error "TODO add parse func"
  | _ -> Ok (parser, left)

and parse_default_infix_expression parser left =
  let prec = get_precidence parser.current in
  let operator = Token.as_string parser.current in
  let%bind parser, right = parse_expression (advance parser) prec in
  Ok (parser, Ast.Infix_Expression { left; operator; right })

and parse_call_expression parser function_ =
  let%bind parser, arguments = parse_expression_list parser Token.Rparen in
  Ok (parser, Ast.Call_Expression { function_; arguments })

and parse_expression_list (parser : t) (end_token : Token.t) =
  let parser = advance parser in
  let rec aux (parser : t) (exprs : Ast.expression list) =
    let%bind parser, expr = parse_expression parser `Lowest in
    let is_end_token = Token.compare parser.peek end_token = 0 in

    match (parser.peek, is_end_token) with
    | _, true ->
        let%bind parser = expect_and_advance parser ~token:end_token in
        Ok (advance parser, List.rev (expr :: exprs))
    | Comma, _ ->
        let parser = advance @@ advance parser in
        aux parser (expr :: exprs)
    | _ -> Error "expected end token or comma"
  in
  match Token.compare parser.current end_token = 0 with
  | true -> Ok (advance parser, [])
  | false -> aux parser []

and parse_prefix_expression parser =
  match parser.current with
  | Bang | Minus -> parse_default_prefix_expression parser
  | Identifier _ -> parse_identifier_expression parser
  | Integer _ -> parse_integer_literal parser
  | String _ -> parse_string_literal parser
  | Lbracket -> Error "TODO add parse func"
  | _ ->
      Error
        ("No PREFIX parse function registered for token "
        ^ Token.as_string parser.current)

and parse_default_prefix_expression parser =
  let prec = get_precidence parser.current in
  let operator = Token.as_string parser.current in
  let%bind parser, right = parse_expression parser prec in
  Ok (parser, Ast.Prefix_Expression { operator; right })

and parse_identifier parser =
  match parser.current with
  | Identifier value -> Ok (parser, value)
  | _ -> Error "expected identifier"

and parse_identifier_expression parser =
  match parser.current with
  | Identifier value -> Ok (parser, Ast.Identifier { value })
  | _ -> Error "expected identifier"

and parse_string_literal parser =
  match parser.current with
  | String value -> Ok (parser, Ast.String_Literal { value })
  | _ -> Error "expected string"

and parse_boolean_literal parser =
  match parser.current with
  | True -> Ok (parser, Ast.Boolean { value = true })
  | False -> Ok (parser, Ast.Boolean { value = false })
  | _ -> Error "expected boolean"

and parse_integer_literal parser =
  match parser.current with
  | Integer value -> Ok (parser, Ast.Integer_Literal { value })
  | _ -> Error "expected integer"
