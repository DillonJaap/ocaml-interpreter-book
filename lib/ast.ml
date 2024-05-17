open! Core

type _node =
  | Program of statement list
  | Statement of statement
  | Expression of expression
[@@deriving sexp_of, compare, show]

and statement =
  | Expression_Statement of { value : expression }
  | Let of { name : identifier; value : expression }
  | Return of { value : expression }
[@@deriving sexp_of, compare, show]

and expression =
  | Identifier of identifier
  | Integer_Literal of int
  | String_Literal of string
  | Boolean of { value : bool }
  | Array_Literal of { value : expression list }
  | Hash_Literal of (expression * expression) list
  | Prefix_Expression of { operator : string; right : expression }
  | Infix_Expression of {
      left : expression;
      operator : string;
      right : expression;
    }
  | If_Expression of {
      condition : expression;
      consequence : block_statement;
      alternate : block_statement;
    }
  | Function_Literal of { parameters : identifier list; body : block_statement }
  | Call_Expression of { function_ : expression; arguments : expression list }
  | Index_Expression of { left : expression; index : expression }
[@@deriving sexp_of, compare, show]

and block_statement = statement list [@@deriving sexp_of, compare, show]

(* todo this complains about two fields with the same name *)
and identifier = string [@@deriving sexp_of, compare, show]
