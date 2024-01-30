type t

val advance : t -> t
val new_lexer : string -> t
val skip_whitespace : t -> t
val read_int : t -> t * int
val read_identifier : t -> t * string
val read_string : t -> t * string
val next_token : t -> t * Token.t

(* Unused *)
val peek_char : t -> char option
val peek_char_is : t -> char -> bool

(* TODO create a version that mutates the lexer *)
