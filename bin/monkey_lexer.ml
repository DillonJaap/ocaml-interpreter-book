open Core

type t =
  { read_pos : int
  ; input : string
  ; ch : char
  }
(*  [@@deriving show] *)

(* update this to return option *)
let advance lex =
  match lex.read_pos with
  | r when r >= String.length lex.input -> None
  | _ ->
    Some { lex with ch = String.get lex.input lex.read_pos; read_pos = lex.read_pos + 1 }
;;

let new_lexer input = advance { read_pos = 0; input; ch = ' ' }

(* Do I need this? *)
let peek_char lex =
  match lex.read_pos with
  | r when r >= String.length lex.input -> None
  | _ -> Some (String.get lex.input lex.read_pos)
;;

(* Do I need this? *)
let peek_char_is lex ch =
  match lex.read_pos with
  | r when r >= String.length lex.input -> false
  | _ -> Char.equal (String.get lex.input lex.read_pos) ch
;;

let rec skip_whitespace lex =
  match lex.ch, advance lex with
  | ch, Some lex' when Char.is_whitespace ch -> skip_whitespace lex'
  | _, None -> None
  | _, _ -> Some lex
;;

let read_int lex =
  let rec aux lex int_ =
    match lex.ch, advance lex with
    | ch, Some lex' when Char.is_digit ch -> aux lex' (int_ ^ String.of_char lex.ch)
    | _, None -> None, int_
    | _, _ -> Some lex, int_
  in
  let lex, str = aux lex "" in
  lex, Int.of_string str
;;

let read_identifier lex =
  let rec aux lex ident =
    match lex.ch, advance lex with
    | ch, Some lex' when Char.is_alpha ch || Char.( = ) ch '_' ->
      aux lex' (ident ^ String.of_char lex.ch)
    | _, None -> None, ident
    | _, _ -> Some lex, ident
  in
  aux lex ""
;;

(* TODO fix this *)
let read_string lex =
  match lex.ch with
  | '"' ->
    let rec aux lex str =
      match advance lex with
      | Some lex' when not (Char.( = ) lex'.ch '"') ->
        aux lex' (str ^ String.of_char lex'.ch)
      | Some lex' -> advance lex', str
      | None -> None, str
    in
    aux lex ""
  | _ -> Some lex, ""
;;

let next_token lex =
  let open Tokens in
  match skip_whitespace lex with
  | None -> None, EOF
  | Some lex ->
    (match lex.ch with
     (* operators *)
     | '=' ->
       (match advance lex with
        | None -> None, EOF
        | Some lex' when Char.( = ) lex'.ch '=' -> advance lex', Equal
        | Some lex' -> advance lex', Assign)
     | '!' ->
       (match advance lex with
        | None -> None, EOF
        | Some lex' when Char.( = ) lex'.ch '=' -> advance lex', Not_Equal
        | Some lex' -> advance lex', Bang)
     | '+' -> advance lex, Plus
     | '-' -> advance lex, Minus
     | '*' -> advance lex, Asterisk
     | '/' -> advance lex, Slash
     | '<' -> advance lex, Less_Than
     | '>' -> advance lex, Great_Than
     (* delimiters *)
     | ',' -> advance lex, Comma
     | ';' -> advance lex, Semicolon
     | '(' -> advance lex, Lparen
     | ')' -> advance lex, Rparen
     | '[' -> advance lex, Lbrace
     | ']' -> advance lex, Rbrace
     | '{' -> advance lex, Lbracket
     | '}' -> advance lex, Rbracket
     (* colon *)
     | ':' -> advance lex, Colon
     (* string *)
     | '"' ->
       let lex, str = read_string lex in
       lex, String str
     (* indentifier *)
     | ch when Char.is_alpha ch ->
       let lex, ident = read_identifier lex in
       lex, look_up_ident ident
     (* integer *)
     | ch when Char.is_digit ch ->
       let lex, int_ = read_int lex in
       lex, Integer int_
     (* other *)
     | _ -> None, Illegal)
;;
