open! Core

type _node =
  | Program of statement list
  | Statement of statement
  | Expression of expression

and statement =
  | Expression_Statement of expression
  | Block_Statement of _block_statement
  | Let of { name : _identifier; value : expression }
  | Return of expression

and expression =
  | Identifier of _identifier
  | Integer_Literal of int64
  | String_Literal of string
  | Boolean of bool
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
      parameters : _identifier list;
      body : _block_statement;
    }
  | Call_Expression of { function_ : expression; arguments : expression list }
  | Index_Expression of { left : expression; index : expression }

and _block_statement = expression list

(* todo this complains about two fields with the same name *)
and _identifier = string
