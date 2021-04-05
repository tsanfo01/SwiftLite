open Ast
open Sast

module StringMap = Map.Make(String)

exception ToDo

let check defns =
  let built_in_decls = 
    let add_bind map name = StringMap.add name {
      fname = name;
      params = [("x", String)];
      ty = Int;
      body = [] } map
    in List.fold_left add_bind StringMap.empty [ "print" ]
  in
  let funcs = built_in_decls in
  let find_func s = 
    try StringMap.find s funcs
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let rec literal = function
    IntLit l -> (Int, SIntLit l)
  | BoolLit l -> (Bool, SBoolLit l)
  | CharLit l -> (Char, SCharLit l)
  | FloatLit l -> (Float, SFloatLit l)
  | StringLit l -> (String, SStringLit l)
  | ArrayLit l -> 
      let ls = List.map literal l in
      let unzip ps = List.fold_right (fun (a, b) (fsts, snds) -> (a::fsts, b::snds)) ps ([], []) in
      let (ts, slits) = unzip ls in
      (* Need to figure out how to allow empty arrays, but this should work for now *)
      let (ty, b) = List.fold_left (fun (t, b) t' -> (t', t = t' && b)) (Int, true) ts
      in if b then (Array ty, SArrayLit slits) else raise (Failure "Polymorphic array not allowed")
  | Nil -> (Optional Int, SNil)
  in
  let rec expr = function 
    Literal l -> 
      let (t, sl) = literal l in (t, SLiteral sl)
  | Call(s, es) as call ->
      let fd = find_func s in
      let param_length = List.length fd.params in
      if List.length es != param_length then
        raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
      else let check_call (_, t') e =
        let (et, e') = expr e in
        let err = "illegal argument found " ^ string_of_type et ^
                " expected " ^ string_of_type fd.ty ^ " in " ^ string_of_expr e
          in ((if et = t' then et else raise (Failure err)), e')
        in let args' = List.map2 check_call fd.params es
      in (fd.ty, SCall(s, args'))
  | _ -> raise ToDo
  in
  let check_stmt = function
    Expr e -> SExpr(expr e)
  | _ -> raise ToDo
  in
  let check_defn = function
    Stmt s  -> SStmt(check_stmt s)
  | _ -> raise ToDo
  in List.map check_defn defns

