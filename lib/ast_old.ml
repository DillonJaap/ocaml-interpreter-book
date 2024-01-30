open! Core

type _node =
  | Program of statement list
  | Statement of statement
  | Expression of expression

and statement =
  | Expression_Statement of { token : Token.t; expression : expression }
  | Block_Statement of _block_statement
  | Let of { token : Token.t; name : _identifier; value : expression }
  | Return of { token : Token.t; value : expression }

and expression =
  | Identifier of _identifier
  | Integer_Literal of { token : Token.t; value : int64 }
  | String_Literal of { token : Token.t; value : string }
  | Boolean of { token : Token.t; value : bool }
  | Array_Literal of { token : Token.t; elements : expression list }
  | Hash_Literal of { token : Token.t; pairst : (expression * expression) list }
  | Prefix_Expression of {
      token : Token.t;
      operator : string;
      right : expression;
    }
  | Infix_Expression of {
      token : Token.t;
      left : expression;
      operator : string;
      right : expression;
    }
  | If_Expression of {
      token : Token.t;
      condition : expression;
      consequence : _block_statement;
      alternate : _block_statement;
    }
  | Function_Literal of {
      token : Token.t;
      parameters : _identifier list;
      body : _block_statement;
    }
  | Call_Expression of {
      token : Token.t;
      function_ : expression;
      arguments : expression list;
    }
  | Index_Expression of {
      token : Token.t;
      left : expression;
      index : expression;
    }

and _block_statement = { token : Token.t; expression : expression list }

(* todo this complains about two fields with the same name *)
and _identifier = { _token : Token.t; value : string }
