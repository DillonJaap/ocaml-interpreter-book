[@@@warning "-30"]

open! Core
open! Ast

module Hashtbl = struct
  include Core.Hashtbl

  let pp pp_key pp_value ppf values =
    Hashtbl.iteri values ~f:(fun ~key ~data ->
        Format.fprintf ppf "@[<1>%a: %a@]@." pp_key key pp_value data)
end

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
  (* | Builtin of (t list -> t) *)
  | Array of t list
  | Hash of (hash_key * hash_pair) list
  | Hash_Element of (hash_key * hash_pair)
[@@deriving show, sexp_of, compare]

and hash_key = Hash_Key of { type_ : t; key : int64 }
[@@deriving show, sexp_of, compare]

and hash_pair = Hash_Pair of { key : t; value : t }
[@@deriving show, sexp_of, compare]
(* TODO make these variants *)
