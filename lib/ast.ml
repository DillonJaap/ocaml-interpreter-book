open! Core

type _node =
  | Program of statement list
  | Statement of statement
  | Expression of expression
[@@deriving sexp_of, compare, show]

and statement =
  | Expression_Statement of { value : expression }
  | Block_Statement of _block_statement
  | Let of { name : identifier; value : expression }
  | Return of { value : expression }
[@@deriving sexp_of, compare, show]

and expression =
  | Identifier of { value : identifier }
  | Integer_Literal of { value : int }
  | String_Literal of { value : string }
  | Boolean of { value : bool }
  | Array_Literal of expression list
  | Hash_Literal of (expression * expression) list
  | Prefix_Expression of { operator : string; right : expression }
  | Infix_Expression of {
      left : expression;
      operator : string;
      right : expression;
    }
  | If_Expression of {
      condition : expression;
      consequence : _block_statement;
      alternate : _block_statement;
    }
  | Function_Literal of {
      parameters : identifier list;
      body : _block_statement;
    }
  | Call_Expression of { function_ : expression; arguments : expression list }
  | Index_Expression of { left : expression; index : expression }
[@@deriving sexp_of, compare, show]

and _block_statement = expression list [@@deriving sexp_of, compare, show]

(* todo this complains about two fields with the same name *)
and identifier = string [@@deriving sexp_of, compare, show]
