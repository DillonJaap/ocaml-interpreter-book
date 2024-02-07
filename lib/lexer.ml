open Core

type t = { read_pos : int; input : string; ch : char option } [@@deriving show]

(* update this to return option *)
let advance lex =
  match lex.read_pos with
  | r when r >= String.length lex.input -> { lex with ch = None }
  | _ ->
      {
        lex with
        ch = Some (String.get lex.input lex.read_pos);
        read_pos = lex.read_pos + 1;
      }

let new_lexer input = advance { read_pos = 0; input; ch = Some ' ' }

(* Do I need this? *)
let peek_char lex =
  match lex.read_pos with
  | r when r >= String.length lex.input -> None
  | _ -> Some (String.get lex.input lex.read_pos)

(* Do I need this? *)
let peek_char_is lex ch =
  match lex.read_pos with
  | r when r >= String.length lex.input -> false
  | _ -> Char.equal (String.get lex.input lex.read_pos) ch

let rec skip_whitespace lex =
  match lex.ch with
  | Some ch when Char.is_whitespace ch -> skip_whitespace (advance lex)
  | Some _ | None -> lex

let read_int lex =
  let rec aux lex int_ =
    match lex.ch with
    | Some ch when Char.is_digit ch ->
        aux (advance lex) (int_ ^ String.of_char ch)
    | Some _ | None -> (lex, int_)
  in
  let lex, str = aux lex "" in
  (lex, Int.of_string str)

let read_identifier lex =
  let rec aux lex ident =
    match lex.ch with
    | Some ch when Char.is_alpha ch ->
        aux (advance lex) (ident ^ String.of_char ch)
    | Some _ | None -> (lex, ident)
  in
  aux lex ""

let read_string lex =
  match lex.ch with
  | Some '"' ->
      let rec aux lex str =
        match lex.ch with
        | Some '"' -> (lex, str)
        | Some ch -> aux (advance lex) (str ^ String.of_char ch)
        | None -> (lex, str)
      in
      aux (advance lex) ""
  | _ -> (lex, "")

let next_token lex =
  let open Token in
  let lex = skip_whitespace lex in
  match lex.ch with
  | None -> (lex, Token.EOF)
  | Some ch -> (
      match ch with
      (* operators *)
      | '=' -> (
          match peek_char lex with
          | None -> (lex, EOF)
          | Some '=' -> (advance (advance lex), Equal)
          | Some _ -> (advance lex, Assign))
      | '!' -> (
          match peek_char lex with
          | None -> (lex, EOF)
          | Some '=' -> (advance (advance lex), Not_Equal)
          | Some _ -> (advance lex, Bang))
      | '+' -> (advance lex, Plus)
      | '-' -> (advance lex, Minus)
      | '*' -> (advance lex, Asterisk)
      | '/' -> (advance lex, Slash)
      | '<' -> (advance lex, Less_Than)
      | '>' -> (advance lex, Greater_Than)
      (* delimiters *)
      | ',' -> (advance lex, Comma)
      | ';' -> (advance lex, Semicolon)
      | '(' -> (advance lex, Lparen)
      | ')' -> (advance lex, Rparen)
      | '{' -> (advance lex, Lbrace)
      | '}' -> (advance lex, Rbrace)
      | '[' -> (advance lex, Lbracket)
      | ']' -> (advance lex, Rbracket)
      (* colon *)
      | ':' -> (advance lex, Colon)
      (* string *)
      | '"' ->
          let lex, str = read_string lex in
          (advance lex, String str)
      (* indentifier *)
      | ch when Char.is_alpha ch ->
          let lex, ident = read_identifier lex in
          (lex, look_up_ident ident)
      (* integer *)
      | ch when Char.is_digit ch ->
          let lex, int_ = read_int lex in
          (lex, Integer int_)
      (* other *)
      | _ -> (advance lex, Illegal))
