[@@@warning "-30"]

open! Core
open! Ast
(*
   INTEGER_OBJ      = "INTEGER"
   BOOLEAN_OBJ      = "BOOLEAN"
   NULL_OBJ         = "NULL"
   RETURN_VALUE_OBJ = "RETURN_VALUE"
   ERROR_OBJ        = "ERROR"
   FUNCTION_OBJ     = "FUNCTION"
   STRING_OBJ       = "STRING"
   BUILTIN_OBJ      = "BUILTIN"
   ARRAY_OBJ        = "ARRAY"
   HASH_OBJ         = "HASH"
*)

type t =
  | Integer of int
  | Boolean of bool
  | Null
  | Return_Value of t
  | Error of string
  | Function of {
      parameters : identifier list;
      body : block_statement; (* TODO add environment here *)
    }
  | String of string
  | Builtin of (t list -> t)
  | Array of t list
  | Hash of (hash_key, hash_pair) Hashtbl.t

and hash_key = { type_ : t; key : int64 }
and hash_pair = { key : t; value : t }
