open! Core
open Object.ObjectMonad

let obj_null = Object.Null

(*Lib.Ast._node -> Environment.t -> Object.T *)
let rec eval node env =
  let open Ast in
  match node with
  | Program n -> eval_program n env
  | Statement stmt -> (
      match stmt with
      | Expression_Statement expr -> eval (Expression expr) env
      | Let l ->
          let value = eval (Expression l.value) env in
          Environment.set env l.name value;
          value (* TODO I think this might be wrong? should return null? *)
      | Return r ->
          let value = eval (Expression r) env in
          Object.Return_Value value
      | Block_Statement statements -> (
          let result =
            List.find_map statements ~f:(fun statement ->
                let result = eval (Statement statement) env in
                match result with
                | Object.Error err -> Some (Object.Error err)
                | Object.Return_Value value -> Some (Object.Return_Value value)
                | _ -> None)
          in
          match result with Some r -> r | None -> Object.Null))
  | Expression expr -> (
      match expr with
      (* Identifier *)
      | Identifier i -> (
          match Environment.get env i with
          | Some res -> res
          | None -> Object.Error "indentifier not found")
      (* Atomic Data Types *)
      | Integer_Literal int -> Object.Integer int
      | String_Literal str -> Object.String str
      | Boolean bool -> Object.Boolean bool
      (* Composite Data Structures *)
      | Array_Literal array ->
          let objs = List.map array ~f:(fun obj -> eval (Expression obj) env) in
          Object.Array objs
      | Hash_Literal h ->
          let object_list =
            List.map h ~f:(fun pair ->
                let key = eval (Expression (fst pair)) env in
                let value = eval (Expression (snd pair)) env in
                Object.Hash_Element
                  ( Object.Hashed_Key { type_ = key; key = Object.hash_of key },
                    Object.Hash_Pair { key; value } ))
          in
          Object.Hash object_list
      | Function_Literal func ->
          let parameters = func.parameters in
          let body = func.body in
          Object.Function { parameters; body }
      (* Composite Expressions *)
      | Prefix_Expression expr -> (
          let right = eval (Expression expr.right) env in
          match (expr.operator, right) with
          | _, Error err -> Error err
          | "!", Object.Boolean true -> Object.Boolean false
          | "!", Object.Boolean false | "!", Object.Null -> Object.Boolean true
          | "!", _ -> Object.Boolean false
          | "-", Object.Integer int -> Object.Integer (-int)
          | "-", obj ->
              Object.Error ("Unknown operator: -" ^ Object.type_to_string obj)
          | prefix_operator, _ ->
              Object.Error ("unknown prefix operator type: " ^ prefix_operator))
      | Infix_Expression expr -> (
          let left = eval (Expression expr.left) env in
          let right = eval (Expression expr.right) env in
          match (left, expr.operator, right) with
          | lobj, operator, robj ->
              let lobj_type = Object.type_to_string lobj in
              let robj_type = Object.type_to_string robj in
              if not (String.equal robj_type lobj_type) then
                Object.Error
                  (sprintf "type mismatch: %s %s %s" robj_type operator
                     lobj_type)
              else
                Object.Error
                  (sprintf "unknown operator: %s %s %s" robj_type operator
                     lobj_type))
      | If_Expression expr ->
          let condition = eval (Expression expr.condition) env in
          if is_truthy condition then
            eval (Statement (Block_Statement expr.consequence)) env
          else eval (Statement (Block_Statement expr.alternate)) env
      | Call_Expression expr ->
          let* function_ = eval (Expression expr.function_) env in
          (* TODO create eval expressions, function use in array as well*)
          let objs =
            List.map expr.arguments ~f:(fun obj -> eval (Expression obj) env)
          in
          (* TODO apply function *)
          (* TODO check length before hand*)
          List.nth_exn objs 0
      | Index_Expression expr -> Object.Null)
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

and is_truthy = function
  | Object.Null | Object.Boolean false -> false
  | Object.Boolean true -> true
  | _ -> true
