open Ast
open Sast

module StringMap = Map.Make(String)

exception ToDo
type scope = Root | WithParent of scope * (typ * bool) StringMap.t

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

  let type_of_identifier s scope =
    let rec lookup = function
        Root -> raise (Failure ("undeclared identifier " ^ s))
      | WithParent(p, map) ->
          try StringMap.find s map
          with Not_found -> lookup p
    in lookup scope
  in

  let rec eqType = function
    (Any, _) -> true
  | (_, Any) -> true
  | (Optional t1, Optional t2) -> eqType (t1, t2)
  | (Array t1, Array t2) -> eqType (t1, t2)
  | (t1, t2) -> t1 = t2
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
      let (ty, b) = List.fold_left (fun (t, b) t' -> (t', eqType (t, t') && b)) (Any, true) ts
      in if b then (Array ty, SArrayLit slits) else raise (Failure "Polymorphic array not allowed")
  | Nil -> (Optional Int, SNil)
  in

  let expr scope e =
    let rec ex = function 
      Literal l -> 
        let (t, sl) = literal l in (t, SLiteral sl)
    | Id(s) -> let (t, _) = type_of_identifier s scope in (t, SId(s))
    | Assign(s, e1) ->
        let (t, e') = ex e1 in
        let (t', b) = type_of_identifier s scope in
        if eqType (t, t') then
          if b then
            raise (Failure ("Variable " ^ s ^ " is constant in local scope"))
          else (t', SAssign(s, (t', e')))
        else raise (Failure ("Illegal assignment of type " ^ string_of_type t ^
                              " to variable of type " ^ string_of_type t'))

    | Call(s, es) as call ->
        let fd = find_func s in
        let param_length = List.length fd.params in
        if List.length es != param_length then
          raise (Failure ("Expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
        else let check_call (_, t') en =
          let (et, e') = ex en in
          let err = "Illegal argument found " ^ string_of_type et ^
                " expected " ^ string_of_type fd.ty ^ " in " ^ string_of_expr en
          in ((if eqType (et, t') then t' else raise (Failure err)), e')
          in let args' = List.map2 check_call fd.params es
        in (fd.ty, SCall(s, args'))
    | _ -> raise ToDo
    in ex e
  in
  let check_stmt scope = function
    Expr e -> (SExpr(expr scope e), scope)
  | Var (n, t, e) ->
      (match scope with
        WithParent(s, map) ->
          let (t', _) as e' = expr scope e in
          let newCurrScope = 
            if StringMap.mem n map
            then raise (Failure ("Name " ^ n ^ " already defined in scope"))
            else StringMap.add n (t, false) map in
          let newScope = WithParent(s, newCurrScope) in
          if eqType (t, t') then (SVar(n, t, e'), newScope) else raise (Failure ("Types of 'var' " ^ n ^ " declaration don't match"))
        | _ -> raise (Failure "Bad scoping"))
  | Let (n, t, e) ->
      (match scope with
        WithParent(s, map) ->
          let (t', _) as e' = expr scope e in
          let newCurrScope = 
            if StringMap.mem n map
            then raise (Failure ("Name " ^ n ^ " already defined in scope"))
            else StringMap.add n (t, true) map in
          let newScope = WithParent(s, newCurrScope) in
          if eqType (t, t') then (SLet(n, t, e'), newScope) else raise (Failure ("Types of 'let' " ^ n ^ " declaration don't match"))
      | _ -> raise (Failure "Bad scoping"))
  | _ -> raise ToDo
  in
  let check_defn scope = function
    Stmt s  -> let (s', newScope) = check_stmt scope s in (SStmt(s'), newScope)
  | _ -> raise ToDo in
  let check_with_scope (newDefns, scope) d =
    let (d', newScope) = check_defn scope d in
    (d'::newDefns, newScope) in
  let (defns', _) = List.fold_left check_with_scope ([], WithParent(Root, StringMap.empty)) defns
  in List.rev defns'

