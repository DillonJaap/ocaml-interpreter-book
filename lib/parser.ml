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
  match Token.compare token parser.peek with 0 -> advance parser | _ -> parser

let expect_and_advance parser ~token =
  match Token.compare token parser.peek with
  | 0 -> Ok (advance parser)
  | _ ->
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
  printf "token: %s\n" (Token.as_string parser.current);
  let%bind parser, lexpr = parse_prefix_expression parser in
  let rec aux parser lexpr =
    let comp = compare_precidence (get_precidence parser.peek) prec in
    match (parser.peek, comp) with
    | Semicolon, _ | _, 0 | _, -1 ->
        print_endline "exit";
        Ok (parser, lexpr)
    | _, 1 ->
        print_endline "I am here";
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
  | Lparen -> Error "TODO add parse func"
  | Lbracket -> Error "TODO add parse func"
  | _ ->
      Error
        ("No INFIX parse function registered for token "
        ^ Token.as_string parser.current)

and parse_default_infix_expression parser left =
  let prec = get_precidence parser.current in
  let operator = Token.as_string parser.current in
  let%bind parser, right = parse_expression (advance parser) prec in
  Ok (parser, Ast.Infix_Expression { left; operator; right })

and parse_call_expression parser function_ =
  let%bind parser, arguments = parse_expression_list parser Token.Rparen in
  Ok (parser, Ast.Call_Expression { function_; arguments })

and parse_expression_list (parser : t) (end_token : Token.t) =
  let rec aux (parser : t) (exprs : Ast.expression list) =
    match parser.peek with
    | Comma ->
        let parser = advance @@ advance parser in
        let%bind parser, expr = parse_expression parser `Lowest in
        aux parser (expr :: exprs)
    | _ ->
        let%bind parser = expect_and_advance parser ~token:end_token in
        Ok (parser, List.rev exprs)
  in
  match Token.compare parser.peek end_token with
  | 0 -> Ok (advance parser, [])
  | _ -> aux parser []

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
  | _ -> Error "expected identifier"

and parse_boolean_literal parser =
  match parser.current with
  | True -> Ok (parser, Ast.Boolean { value = true })
  | False -> Ok (parser, Ast.Boolean { value = false })
  | _ -> Error "expected identifier"

and parse_integer_literal parser =
  match parser.current with
  | Integer value -> Ok (parser, Ast.Integer_Literal { value })
  | _ -> Error "expected identifier"
