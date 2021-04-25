open Ast
open Sast

module StringMap = Map.Make(String)

exception ToDo
type scope = Root | WithParent of scope * (typ * bool) StringMap.t

let check defns =
  let built_in_decls = 
    let add_bind map (name, t) = StringMap.add name {
      fname = name;
      params = [("x", t)];
      ty = Int;
      body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print", String); ("printi", Int); ("printb", Bool); ("printc", Char) ]
  in
  
  let find_func funcs s = 
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

  let assignTypeEq = function
    (Optional t1, t2) as ts -> eqType (t1, t2) || eqType ts
  | (t1, t2) -> eqType (t1, t2) in

  let rec literal = function
    IntLit l -> (Int, SIntLit l)
  | BoolLit l -> (Bool, SBoolLit l)
  | CharLit l -> (Char, SCharLit l)
  | FloatLit l -> (Float, SFloatLit l)
  | StringLit l -> (String, SStringLit l)
  | ArrayLit (t, l) -> 
      let ls = List.map literal l in
      let unzip ps = List.fold_right (fun (a, b) (fsts, snds) -> (a::fsts, b::snds)) ps ([], []) in
      let (ts, slits) = unzip ls in
      let (ty, b) = List.fold_left (fun (t', b) t'' -> (t', eqType (t', t'') && b)) (t, true) ts
      in if b then (Array ty, SArrayLit (t, slits)) else raise (Failure "Polymorphic array not allowed")
  | Nil t -> (Optional t, SNil t)
  in

  let expr scope funcs e =
    let rec ex = function 
      Literal l -> 
        let (t, sl) = literal l in (t, SLiteral sl)
    | Id(s) -> let (t, _) = type_of_identifier s scope in (t, SId(s))
    | Assign(s, e1) ->
        let (t, e') = ex e1 in
        let (t', b) = type_of_identifier s scope in
        if assignTypeEq (t', t) then
          if b then
            raise (Failure ("Variable " ^ s ^ " is constant in local scope"))
          else (t', SAssign(s, (t', e')))
        else raise (Failure ("Illegal assignment of type " ^ string_of_type t ^
                              " to variable of type " ^ string_of_type t'))
    | Binop(e1, op, e2) ->
        let (t1, e1') = ex e1 
        and (t2, e2') = ex e2 in

        let same = eqType(t1, t2) in
        let ty =
          match op with
            Add | Sub | Mul | Div | Mod when same && t1 = Int   -> Int
          | Add | Sub | Mul | Div when same && t1 = Float -> Float
          | Eq | Neq            when same               -> Bool
          | Lt | Leq | Gt | Geq
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | Add when same && t1 = String -> String
          | Orange | Crange when same && t1 = Int -> Array(Int)
          | _ -> raise (
                    Failure ("illegal binary operator " ^
                       string_of_type t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_type t2 ^ " in " ^ string_of_expr e))
        in (ty, SBinop((t1, e1'), op, (t2, e2')))
    | Unop (op, e) ->
        let (t, e') = ex e in
        let ty = match op with
          Neg when t = Int || t = Float -> t
        | Not when t = Bool -> Bool
        | _ -> raise (Failure ("illegal unary operator " ^ 
                               string_of_uop op ^ string_of_type t ^
                               " in " ^ string_of_expr e))
        in (ty, SUnop(op, (t, e')))
    | ArrAt(a, i) ->
        let (ta, a') = ex a in
        let (ti, i') = ex i in
        if eqType (ti, Int) then
          match ta with
            Array t' -> (t', SArrAt((ta, a'), (ti, i')))
          | fail -> raise (Failure ("Cannot access index of an expression of type " ^ string_of_type fail))
        else raise (Failure ("Expected index of type Int, instead got " ^ string_of_type ti))
    | ArrAssign(a, i, e) ->
        let (ta, a') = ex a in
        let (ti, i') = ex i in
        let (te, e') = ex e in
        if eqType (ti, Int) then
          match ta with
            Array t' ->
              if eqType (t', te) then (te, SArrAssign((ta, a'), (ti, i'), (te, e')))
              else raise (Failure ("Array element types must match assignment, instead got " ^
                string_of_type ta ^ " and " ^ string_of_type te))
          | fail -> raise (Failure ("Cannot assign to index of an expression of type " ^ string_of_type fail))
        else raise (Failure ("Expected index of type Int, instead got " ^ string_of_type ti))
    | Call(s, es) as call ->
        let fd = find_func funcs s in
        let param_length = List.length fd.params in
        if List.length es != param_length then
          raise (Failure ("Expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
        else let check_call (_, t') en =
          let (et, e') = ex en in
          let err = "Illegal argument found " ^ string_of_type et ^
                " expected " ^ string_of_type t' ^ " in " ^ string_of_expr en
          in ((if eqType (et, t') then t' else raise (Failure err)), e')
          in let args' = List.map2 check_call fd.params es
        in (fd.ty, SCall(s, args'))
    | Opt(e) ->
        let (t, e') = ex e in
        (Optional t, SOpt(t, e'))
    | _ -> raise ToDo
    in ex e
  in

  let check_toplevel_block_with_check check = 
    let rec check_toplevel_block scope funcs block = 
      let check_stmt_in_block (ss, scope) s =
        let (s', scope') = check scope funcs check_toplevel_block s in (s' :: ss, scope') in
        List.fold_left check_stmt_in_block ([], scope) block
    in check_toplevel_block
  in
  
  let stmt scope funcs check_block s =
    let rec check_stmt scope funcs = function

      Expr e -> (SExpr(expr scope funcs e), scope)
    | Var (n, t, e) ->
        (match scope with
          WithParent(s, map) ->
            let (t', _) as e' = expr scope funcs e in
            let newCurrScope = 
              if StringMap.mem n map
              then raise (Failure ("Name " ^ n ^ " already defined in scope"))
              else StringMap.add n (t, false) map in
            let newScope = WithParent(s, newCurrScope) in
            if assignTypeEq (t, t') then (SVar(n, t, e'), newScope) else raise (Failure ("Types of 'var' " ^ n ^ " declaration don't match"))
          | _ -> raise (Failure "Bad scoping"))
    | Let (n, t, e) ->
        (match scope with
          WithParent(s, map) ->
            let (t', _) as e' = expr scope funcs e in
            let newCurrScope = 
              if StringMap.mem n map
              then raise (Failure ("Name " ^ n ^ " already defined in scope"))
              else StringMap.add n (t, true) map in
            let newScope = WithParent(s, newCurrScope) in
            if assignTypeEq (t, t') then (SLet(n, t, e'), newScope) else raise (Failure ("Types of 'let' " ^ n ^ " declaration don't match"))
        | _ -> raise (Failure "Bad scoping"))
    | Return e -> check_stmt scope funcs (Expr e)
    | If (c, tblock, fblock) ->
        let (t, e) = expr scope funcs c in
        let (tblock', _) = check_block (WithParent(scope, StringMap.empty)) funcs tblock in
        let (fblock', _) = check_block (WithParent(scope, StringMap.empty)) funcs fblock in
        if t = Bool then (SIf((t, e), List.rev tblock', List.rev fblock'), scope) else raise (Failure "'if' condition must be a boolean")
    | While (c, body) ->
        let (t, e) = expr scope funcs c in
        let (body', _) = check_block (WithParent(scope, StringMap.empty)) funcs body in
        if t = Bool then (SWhile((t, e), List.rev body'), scope) else raise (Failure "'loop' condition must be a boolean")
    | For (s, e, body) ->
        let (t, e') = expr scope funcs e in
        let t' = match t with Array t' -> t' | _ -> raise (Failure ("Expression in for loop must be an array")) in
        let newScope = WithParent(scope, StringMap.add s (t', true) StringMap.empty) in
        let (body', _) = check_block newScope funcs body in
        (SFor(s, (t, e'), List.rev body'), scope)
    | _ -> raise ToDo
  in check_stmt scope funcs s
  in

  let check_binds (kind : string) (to_check : (string * typ) list) = 
    let name_compare (n1, _) (n2, _) = compare n1 n2 in
    let check_it checked binding = 
      let dup_err = "duplicate " ^ kind ^ " " ^ fst binding
      in match binding with
      | (n1, _) -> match checked with
                    (* No duplicate bindings *)
                      ((n2, _) :: _) when n1 = n2 -> raise (Failure dup_err)
                    | _ -> binding :: checked
    in let _ = List.fold_left check_it [] (List.sort name_compare to_check) 
    in to_check
  in

  let check_function_block_with_check check =
    let rec check_function_block scope funcs block =
      let rec check_stmt_list scope = function
        [Return _ as s] -> let (s', newScope) = check scope funcs check_function_block s in ([s'], newScope)
      | Return _ :: _   -> raise (Failure "nothing may follow a return")
      | s :: ss         ->
          let (s', newScope) = check scope funcs check_function_block s in
          let (ss', newScope') = check_stmt_list newScope ss in
          (s' :: ss', newScope')
      | []              -> ([], scope)
      in let (ss, newScope) = check_stmt_list scope block in (List.rev ss, newScope)
    in check_function_block
  in

  let check_func scope funcs f =
    let fname = f.fname in
    let params' = check_binds "formal" f.params in
    let newScope = WithParent(scope, List.fold_left (fun m (s, t) -> StringMap.add s (t, false) m) StringMap.empty params') in
    let newFuncs = StringMap.add fname f funcs in
    let check_fstmt scope funcs check_block = function
        Return e ->
          let (t, e') = expr scope funcs e in
          if eqType (t, f.ty) then (SReturn(t, e'), scope)
          else raise (Failure("return gives " ^ string_of_type t ^ " expected " ^
                        string_of_type f.ty ^ " in " ^ string_of_expr e))
      | s -> stmt scope funcs check_block s in
    let (ss, _) = (check_function_block_with_check check_fstmt) (WithParent(newScope, StringMap.empty)) newFuncs f.body in
    ({
      sfname = fname;
      sparams = params';
      sty = f.ty;
      sbody = List.rev ss;
    }, newFuncs) in




  let check_defn scope funcs = function
    Stmt s  -> let (s', newScope) = stmt scope funcs (check_toplevel_block_with_check stmt) s in (SStmt(s'), newScope, funcs)
  | Func_defn f -> let (f', newFuncs) = check_func scope funcs f in (SFunc_defn f', scope, newFuncs)
  | _ -> raise ToDo in
  let check_with_scope (newDefns, scope, funcs) d =
    let (d', newScope, newFuncs) = check_defn scope funcs d in
    (d'::newDefns, newScope, newFuncs) in
  let (defns', _, _) = List.fold_left check_with_scope ([], WithParent(Root, StringMap.empty), built_in_decls) defns
  in List.rev defns'

