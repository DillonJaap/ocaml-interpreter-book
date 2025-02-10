open! Core

let true_ = Object.Boolean true
let false_ = Object.Boolean false
let null = Object.Null

let rec eval node env =
  match node with
  | Ast.Program n -> eval_program n env
  | Ast.Statement stmt -> (
      match stmt with
      | Ast.Expression_Statement expr -> eval (Expression expr) env
      | Ast.Let l ->
          let value = eval (Ast.Expression l.value) env in
          Environment.set env l.name value;
          value
      | Ast.Return r ->
          let value = eval (Ast.Expression r) env in
          Object.Return_Value value)
  | Ast.Expression expr -> (
      match expr with
      | Ast.Integer_Literal l -> Object.Integer l
      | Ast.String_Literal l -> Object.String l
      | Ast.Boolean b -> Object.Boolean b
      | Ast.Array_Literal a ->
          let objs = List.map a ~f:(fun obj -> eval (Ast.Expression obj) env) in
          Object.Array objs
      | Ast.Hash_Literal h ->
          let object_list =
            List.map h ~f:(fun pair ->
                let key = eval (Ast.Expression (fst pair)) env in
                let value = eval (Ast.Expression (snd pair)) env in
                (key, value)
          in
          Object.Hash object_list
      (* | Ast.Identifier i -> Object. i *)
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
