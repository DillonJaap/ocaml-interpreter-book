type t

val advance : t -> t option
val new_lexer : string -> t option
val skip_whitespace : t -> t option
val read_int : t -> t option * int
val read_identifier : t -> t option * string
val read_string : t -> t option * string
val next_token : t -> t option * Tokens.t

(* Unused *)
val peek_char : t -> char option
val peek_char_is : t -> char -> bool

(* TODO create a version that mutates the lexer *)
