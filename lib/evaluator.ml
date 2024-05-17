open! Core

let true_ = Object.Boolean true
let false_ = Object.Boolean false
let null = Object.Null

let rec eval node env =
  match node with
  | Ast.Program n -> eval_program n env
  | Ast.Statement stmt -> (
      match stmt with
      | Ast.Expression_Statement v -> eval (Expression v.value) env
      | _ -> null)
  | Ast.Expression expr -> (
      match expr with
      | Ast.Integer_Literal l -> Object.Integer l
      | Ast.String_Literal l -> Object.String l
      | _ -> null)
(* TODO should I return an option type *)

and eval_program stmts env =
  let rec aux (stmts : Ast.statement list) env res =
    match stmts with
    | hd :: tl -> (
        let ret = eval (Ast.Statement hd) env in
        match ret with
        | Object.Error _ | Object.Return_Value _ -> ret
        | _ -> ( match tl with [] -> ret | tl -> aux tl env res))
    | _ -> res
  in
  aux stmts env Object.Null
