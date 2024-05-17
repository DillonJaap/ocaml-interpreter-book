open! Core

type t = { store : (string, Object.t) Hashtbl.t; outer : t option }

let new_ () = { store = Hashtbl.create (module String); outer = None }
let new_enclosed outer = { store = Hashtbl.create (module String); outer }

let rec get env (key : string) =
  let res = Hashtbl.find env.store key in
  match (res, env.outer) with
  | None, None -> None
  | None, Some outer -> get outer key
  | Some k, _ -> Some k

let set env key data = Hashtbl.set env.store ~key ~data
