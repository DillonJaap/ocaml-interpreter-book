open Core

type t =
  | Illegal
  | EOF
  (* Identifies and Literals *)
  | Identifier of string
  | Integer of int
  (* Operators *)
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | Less_Than
  | Great_Than
  (* Delimiters *)
  | Comma
  | Semicolon
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  | Lbracket
  | Rbracket
  (* Keywords *)
  | Function
  | Let
  | True
  | False
  | If
  | Else
  | Return
  (* equality operators *)
  | Equal
  | Not_Equal
  (* other *)
  | String of string
  | Colon

let as_string = function
  | Illegal -> "Illegal"
  | EOF -> "EOF"
  (* Identifies and Literals *)
  | Identifier s -> s
  | Integer i -> sprintf "%d" i
  (* Operators *)
  | Assign -> "="
  | Bang -> "!"
  | Plus -> "+"
  | Minus -> "-"
  | Asterisk -> "*"
  | Slash -> "/"
  | Less_Than -> "<"
  | Great_Than -> ">"
  (* Delimiters *)
  | Comma -> ","
  | Semicolon -> ";"
  | Lparen -> "("
  | Rparen -> ")"
  | Lbrace -> "{"
  | Rbrace -> "}"
  | Lbracket -> "["
  | Rbracket -> "]"
  (* Keywords *)
  | Function -> "Function"
  | Let -> "Let"
  | True -> "True"
  | False -> "False"
  | If -> "If"
  | Else -> "Else"
  | Return -> "Return"
  (* equality operators *)
  | Equal -> "=="
  | Not_Equal -> "!="
  (* other *)
  | String s -> sprintf {|"%s"|} s
  | Colon -> ":"
;;

let look_up_ident ident =
  match ident with
  | "fn" -> Function
  | "let" -> Let
  | "true" -> True
  | "false" -> False
  | "if" -> If
  | "else" -> Else
  | "return" -> Return
  | _ -> Identifier ident
;;
