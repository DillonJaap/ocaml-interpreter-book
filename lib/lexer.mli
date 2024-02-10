type t [@@deriving show]

val advance : t -> t
val new_lexer : string -> t
val skip_whitespace : t -> t
val read_int : t -> t * int
val read_identifier : t -> t * string
val read_string : t -> t * string
val next_token : t -> t * Token.t
val peek_char : t -> char option

(* TODO create a version that mutates the lexer *)
