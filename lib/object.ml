[@@@warning "-30"]

open! Core

(*module Hashtbl = struct*)
(*  include Core.Hashtbl*)
(**)
(*  let pp pp_key pp_value ppf values =*)
(*    Hashtbl.iteri values ~f:(fun ~key ~data ->*)
(*        Format.fprintf ppf "@[<1>%a: %a@]@." pp_key key pp_value data)*)
(*end*)

type t =
  | Integer of int
  | Boolean of bool
  | Null
  | Return_Value of t
  | Error of string
  | Function of {
      parameters : Ast.identifier list;
      body : Ast.block_statement; (* TODO add environment here *)
    }
  | String of string
  (* | Builtin of (t list -> t) *)
  | Array of t list
  | Hash of hash_element list
[@@deriving show, sexp_of, compare]

and hash_element = Hash_Element of hashed_key * hash_pair
[@@deriving show, sexp_of, compare]

and hashed_key = Hashed_Key of { type_ : t; key : int64 }
[@@deriving show, sexp_of, compare]

and hash_pair = Hash_Pair of { key : t; value : t }
[@@deriving show, sexp_of, compare]

(* TODO add all hash functions *)
let hash_of obj = match obj with Integer i -> Int64.of_int i | _ -> 1L

let type_to_string (obj : t) : string =
  match obj with
  | Integer _ -> "integer"
  | Boolean _ -> "boolean"
  | Null -> "null"
  | Return_Value _ -> "return_value"
  | Error _ -> "error"
  | Function _ -> "function"
  | String _ -> "string"
  | Array _ -> "array"
  | Hash _ -> "hash"

module ObjectMonad = struct
  let bind obj f = match obj with Error x -> Error x | other -> f other
  let return x = x
  let ( let* ) = bind
end
