type token =
  | Illegal
  | EOF
  (* Identifies and Literals *)
  | Identifier
  | Integer
  (* Operators *)
  | Assign
  | Plus
  | Minus
  | Asterisk
  | Slach
  | Divide
  | Less_Than
  | Great_Than
  (* Delimters *)
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
  | Eq
  | Not_eq
  | String
  | Colon
