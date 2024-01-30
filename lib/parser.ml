open! Core

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

let expect_peek parser token =
  let lexer, peek_token = Lexer.next_token parser.lexer in
  ( peek_token,
    { parser with current_token = parser.peek_token; lexer; peek_token } )

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

type rule =
  | Must of Token.t
  | Opt of Token.t
  | Expr of Ast.expression

let check_rules parser  rules = 
  let check_rule parser rule =
    match rule with 
    | Must token  -> parser.current_token
    | Opt token -> token
    | Expr token -> 
  in
let rec aux parser  rules = 
match rules with 
    | Must token :: tl -> 
        | Must 
        in
        aux parser rules

(* TODO do I need to have a parse identifier function here? *)
let rec parse_program parser =
  let open Ast in
  let rec aux parser (statements : statement list) =
    let _, token = Lexer.next_token parser.lexer in
    match token with
    | Let -> (
        let stmt, parser' = parse_let_statement parser in
        match stmt with
        | Some l -> aux (advance parser') (l :: statements)
        | None -> aux (advance parser') statements)
    | Return -> statements
    | EOF -> statements
    | _ -> statements
  in
  aux parser [] |> List.rev

and parse_expression _parser =
  let open Ast in
  Identifier "TODO remove this"

and parse_let_statement parser =
  let open Ast in
  let tokens, parser' = peek_n_tokens parser 2 in
  let expr = parse_expression parser' in
  match tokens with
  | [ Identifier ident; Assign ] -> (
      let let_stmt = Some (Let { name = ident; value = expr }) in
      match peek_token parser' with
      | Semicolon, p -> (let_stmt, p)
      | _ -> (let_stmt, parser'))
  | _ -> (None, parser)

(*
   match tokens with
   | [ Identifier ident; Assign ] ->
       (Some (Let { name = ident; value = expr }), parser')
   | _ -> (None, parser)
*)

and parse_return_statement parser =
  let open Ast in
  let tokens, parser' = peek_n_tokens parser 2 in
  match tokens with
  | [ Identifier ident; Assign ] ->
      let expr = parse_expression parser' in
      (Some (Return expr), parser')
  | _ -> (None, parser)

(*
   and parse_let_statement parser =
     match parser.peek_token with
     | Identifier i ->
         Some
           (Let
              {
                token = parser.current_token;
                name = { _token = parser.current_token; value = i };
                value = Identifier { _token = parser.current_token; value = i };
              })
     | _ -> None
*)
