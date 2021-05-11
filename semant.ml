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
        Root -> raise Not_found
      | WithParent(p, map) ->
          try StringMap.find s map
          with Not_found -> lookup p
    in lookup scope
  in

  let rec index_of x = function
    [] -> raise (Failure "Not Found")
  | h :: t -> if x = h then 0 else 1 + index_of x t in

  let unzip ps = List.fold_right (fun (a, b) (fsts, snds) -> (a::fsts, b::snds)) ps ([], []) in

  let rec literal = function
    IntLit l -> (Int, SIntLit l)
  | BoolLit l -> (Bool, SBoolLit l)
  | CharLit l -> (Char, SCharLit l)
  | FloatLit l -> (Float, SFloatLit l)
  | StringLit l -> (String, SStringLit l)
  | ArrayLit (t, l) -> 
      let ls = List.map literal l in
      let (ts, slits) = unzip ls in
      let (ty, b) = List.fold_left (fun (t', b) t'' -> (t', t' = t'' && b)) (t, true) ts
      in if b then (Array ty, SArrayLit (t, slits)) else raise (Failure "Polymorphic array not allowed")
  | Nil t -> (Optional t, SNil t)
  in

  let expr scope funcs enums classes e =
    let rec ex = function 
      Literal l -> 
        let (t, sl) = literal l in (t, SLiteral sl)
    | Id(s) -> let (t, _) = try type_of_identifier s scope with Not_found -> raise (Failure ("undeclared identifier " ^ s)) in (t, SId(s))
    | Assign(s, e1) ->
        let (t, e') = ex e1 in
        let (t', b) = try type_of_identifier s scope with Not_found -> raise (Failure ("undeclared identifier " ^ s)) in
        if t = t' then
          if b then
            raise (Failure ("Variable " ^ s ^ " is constant in local scope"))
          else (t', SAssign(s, (t', e')))
        else raise (Failure ("Illegal assignment of type " ^ string_of_type t ^
                              " to variable of type " ^ string_of_type t'))
    | Binop(e1, op, e2) ->
        let (t1, e1') = ex e1 
        and (t2, e2') = ex e2 in

        let same = t1 = t2 in
        let ty =
          match op with
            Add | Sub | Mul | Div | Mod when same && t1 = Int   -> Int
          | Add | Sub | Mul | Div when same && t1 = Float -> Float
          | Eq | Neq            when same && (match t1 with Int | Float | Bool | Char -> true | UserDef(s) -> StringMap.mem s enums | _ -> false)   -> Bool
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
        if ti = Int then
          match ta with
            Array t' -> (t', SArrAt((ta, a'), (ti, i')))
          | fail -> raise (Failure ("Cannot access index of an expression of type " ^ string_of_type fail))
        else raise (Failure ("Expected index of type Int, instead got " ^ string_of_type ti))
    | ArrAssign(a, i, e) ->
        let (ta, a') = ex a in
        let (ti, i') = ex i in
        let (te, e') = ex e in
        if ti = Int then
          match ta with
            Array t' ->
              if t' = te then (te, SArrAssign((ta, a'), (ti, i'), (te, e')))
              else raise (Failure ("Array element types must match assignment, instead got " ^
                string_of_type ta ^ " and " ^ string_of_type te))
          | fail -> raise (Failure ("Cannot assign to index of an expression of type " ^ string_of_type fail))
        else raise (Failure ("Expected index of type Int, instead got " ^ string_of_type ti))
    | Call(s, es) as call ->
      if StringMap.mem s classes then
        let c = StringMap.find s classes in
        let inits = c.inits in
        let es' = List.map ex es in
        let (tys, _) = List.split es' in
        let eqArgTys (args, _) =
          let (_, tys') = List.split args in
          tys = tys'
        in
        let i = try List.find eqArgTys inits with Not_found -> raise (Failure ("no init matching given argument types")) in
        let si = s ^ (string_of_int (index_of i inits)) in
        (UserDef s, SInit (s, si, es'))
      else
        let fd = try find_func funcs s with Not_found -> raise (Failure ("no such function " ^ s)) in
        let param_length = List.length fd.params in
        if List.length es != param_length then
          raise (Failure ("Expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
        else let check_call (_, t') en =
          let (et, e') = ex en in
          let err = "Illegal argument found " ^ string_of_type et ^
                " expected " ^ string_of_type t' ^ " in " ^ string_of_expr en
          in ((if et = t' then t' else raise (Failure err)), e')
          in let args' = List.map2 check_call fd.params es
        in (fd.ty, SCall(s, args'))
    | Opt(e) ->
        let (t, e') = ex e in
        (Optional t, SOpt(t, e'))
    | EnumCase(s1, s2) ->
        let e = try StringMap.find s1 enums with Not_found -> raise (Failure ("Enum " ^ s1 ^ " not defined")) in
        if List.mem s2 e.cases then
        (UserDef(e.ename), SEnumCase(s1, s2))
      else raise (Failure ("Enum " ^ s1 ^ " has no case " ^ s2))
    | MethodCall(s, s', es) ->
        let t = try type_of_identifier s scope with Not_found -> raise (Failure ("unbound identifier " ^ s)) in
        let c = match t with (UserDef t, _) -> (try StringMap.find t classes
                                                  with Not_found -> raise (Failure (s ^ " is not an object")))
                           | _ -> raise (Failure (s ^ " is not an object")) in
        let m = try List.find (fun m -> m.fname = s') c.methods with Not_found -> raise (Failure ("class " ^ c.cname ^ " has no method " ^ s')) in
        let es' = List.map ex es in
        let offset = List.length c.fields + (index_of m c.methods) in
        (m.ty, SMethodCall(s, s', offset, es'))
    | Self ->
        let (ty, _) = try type_of_identifier "self" scope with Not_found -> raise (Failure("unexpected self at toplevel")) in
        let _ = match ty with UserDef _ -> () | _ -> raise (Failure ("bad type of self")) in
        (ty, SSelf)
    | SelfCall (s, es) ->
        let ty = try type_of_identifier "self" scope with Not_found -> raise (Failure("unexpected self at toplevel")) in
        let cname = match ty with (UserDef s, _) -> s | _ -> raise (Failure ("bad type of self")) in
        let c = StringMap.find cname classes in
        let m = try List.find (fun m -> m.fname = s) c.methods with Not_found -> raise (Failure ("class " ^ c.cname ^ " has no method " ^ s)) in
        let es' = List.map ex es in
        let offset = List.length c.fields + (index_of m c.methods) in
        (m.ty, SSelfCall(s, offset, es'))
    | SelfField s ->
        let ty = try type_of_identifier "self" scope with Not_found -> raise (Failure("unexpected self at toplevel")) in
        let cname = match ty with (UserDef s, _) -> s | _ -> raise (Failure ("bad type of self")) in
        let c = StringMap.find cname classes in
        let (_, t, _) = try List.find (fun (s', _, _) -> s = s') c.fields with Not_found -> raise (Failure ("class " ^ c.cname ^ " has no field " ^ s)) in
        let offset = index_of s (List.map (fun (a, _, _) -> a) c.fields) in
        (t, SSelfField (s, offset))
    | SelfAssign (s, e) ->
        let ty = try type_of_identifier "self" scope with Not_found -> raise (Failure("unexpected self at toplevel")) in
        let cname = match ty with (UserDef s, _) -> s | _ -> raise (Failure ("bad type of self")) in
        let c = StringMap.find cname classes in
        let (_, t, _) = try List.find (fun (s', _, _) -> s = s') c.fields with Not_found -> raise (Failure ("class " ^ c.cname ^ " has no field " ^ s)) in
        let (t', e') = ex e in
        let offset = index_of s (List.map (fun (a, _, _) -> a) c.fields) in
        if t = t' then (t, SSelfAssign(s, offset, (t', e'))) else raise (Failure ("bad field assign, expected " ^ string_of_type t ^ " got " ^ string_of_type t'))
    | NoExpr -> (Int, SNoExpr)
    in ex e
  in

  let check_toplevel_block_with_check check = 
    let rec check_toplevel_block scope funcs enums classes block = 
      let check_stmt_in_block (ss, scope) s =
        let (s', scope') = check scope funcs enums classes check_toplevel_block s in (s' :: ss, scope') in
        List.fold_left check_stmt_in_block ([], scope) block
    in check_toplevel_block
  in
  
  let stmt scope funcs enums classes check_block s =
    let rec check_stmt scope funcs enums classes = function

      Expr e -> (SExpr(expr scope funcs enums classes e), scope)
    | Var (n, t, e) ->
        (match scope with
          WithParent(s, map) ->
            let (t', _) as e' = expr scope funcs enums classes e in
            let newCurrScope = 
              if StringMap.mem n map
              then raise (Failure ("Name " ^ n ^ " already defined in scope"))
              else StringMap.add n (t, false) map in
            let newScope = WithParent(s, newCurrScope) in
            if t = t' then (SVar(n, t, e'), newScope) else raise (Failure ("Types of 'var' " ^ n ^ " declaration don't match"))
          | _ -> raise (Failure "Bad scoping"))
    | Let (n, t, e) ->
        (match scope with
          WithParent(s, map) ->
            let (t', _) as e' = expr scope funcs enums classes e in
            let newCurrScope = 
              if StringMap.mem n map
              then raise (Failure ("Name " ^ n ^ " already defined in scope"))
              else StringMap.add n (t, true) map in
            let newScope = WithParent(s, newCurrScope) in
            if t = t' then (SLet(n, t, e'), newScope) else raise (Failure ("Types of 'let' " ^ n ^ " declaration don't match"))
        | _ -> raise (Failure "Bad scoping"))
    | Return e -> check_stmt scope funcs enums classes (Expr e)
    | If (c, tblock, fblock) ->
        let (t, e) = expr scope funcs enums classes c in
        let (tblock', _) = check_block (WithParent(scope, StringMap.empty)) funcs enums classes tblock in
        let (fblock', _) = check_block (WithParent(scope, StringMap.empty)) funcs enums classes fblock in
        if t = Bool then (SIf((t, e), List.rev tblock', List.rev fblock'), scope) else raise (Failure "'if' condition must be a boolean")
    | IfLet (s, t, e, tblock, fblock) ->
        let (te, e') = expr scope funcs enums classes e in
        let t' = match te with Optional t' -> t' | t' -> raise (Failure("Expected optional in if let, instead got " ^ string_of_type t')) in
        let someScope = StringMap.add s (t, true) StringMap.empty in
        let (tblock', _) = check_block (WithParent(scope, someScope)) funcs enums classes tblock in
        let (fblock', _) = check_block (WithParent(scope, StringMap.empty)) funcs enums classes fblock in
        if t' = t then (SIfLet(s, t, (te, e'), List.rev tblock', List.rev fblock'), scope) else raise (Failure ("Types of optional and id in if let must match, instead got " ^ string_of_type t ^ " and " ^ string_of_type t'))
    | While (c, body) ->
        let (t, e) = expr scope funcs enums classes c in
        let (body', _) = check_block (WithParent(scope, StringMap.empty)) funcs enums classes body in
        if t = Bool then (SWhile((t, e), List.rev body'), scope) else raise (Failure "'loop' condition must be a boolean")
    | For (s, e, body) ->
        let (t, e') = expr scope funcs enums classes e in
        let t' = match t with Array t' -> t' | _ -> raise (Failure ("Expression in for loop must be an array")) in
        let newScope = WithParent(scope, StringMap.add s (t', true) StringMap.empty) in
        let (body', _) = check_block newScope funcs enums classes body in
        (SFor(s, (t, e'), List.rev body'), scope)
  in check_stmt scope funcs enums classes s
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
    let rec check_function_block scope funcs enums classes block =
      let rec check_stmt_list scope = function
        [Return _ as s] -> let (s', newScope) = check scope funcs enums classes check_function_block s in ([s'], newScope)
      | Return _ :: _   -> raise (Failure "nothing may follow a return")
      | s :: ss         ->
          let (s', newScope) = check scope funcs enums classes check_function_block s in
          let (ss', newScope') = check_stmt_list newScope ss in
          (s' :: ss', newScope')
      | []              -> ([], scope)
      in let (ss, newScope) = check_stmt_list scope block in (List.rev ss, newScope)
    in check_function_block
  in

  let check_fstmt ty scope funcs enums classes check_block = function
      Return e ->
        let (t, e') = expr scope funcs enums classes e in
        if t = ty then (SReturn(t, e'), scope)
        else raise (Failure("return gives " ^ string_of_type t ^ " expected " ^
                      string_of_type ty ^ " in " ^ string_of_expr e))
    | s -> stmt scope funcs enums classes check_block s in

  let check_func scope funcs enums classes check_stmt f =
    let fname = f.fname in
    let params' = check_binds "formal" f.params in
    let newScope = WithParent(scope, List.fold_left (fun m (s, t) -> StringMap.add s (t, false) m) StringMap.empty params') in
    let newFuncs = StringMap.add fname f funcs in
    let (ss, _) = (check_function_block_with_check (check_stmt f.ty)) (WithParent(newScope, StringMap.empty)) newFuncs enums classes f.body in
    ({
      sfname = fname;
      sparams = params';
      sty = f.ty;
      sbody = List.rev ss;
    }, newFuncs) in

  let check_enum enums classes e =
    let check_it checked name = 
      let dup_err = "duplicate enum case " ^ name
      in match name with
      | n1 -> match checked with
                    (* No duplicate bindings *)
                      n2 :: _ when n1 = n2 -> raise (Failure dup_err)
                    | _ -> name :: checked
    in let _ = List.fold_left check_it [] (List.sort compare e.cases)
    in let _ = if StringMap.mem e.ename enums || StringMap.mem e.ename classes
               then raise (Failure ("Enum " ^ e.ename ^ " already defined"))
               else ()
    in ({ sename = e.ename ; scases = e.cases }, StringMap.add e.ename e enums)
  in

  let check_inits_ps is =
    let check_init_params checked ps =
      match checked with
        [] -> ps :: checked
      | ps' :: _ -> if ps = ps' then raise (Failure ("duplicate inits")) else ps :: checked
    in List.fold_left check_init_params [] (List.sort compare (List.map (fun (a, _) -> List.map (fun (_, b) -> b) a) is))
  in

  let check_init scope funcs enums classes c (binds, i) =
    let newScope = WithParent(scope, List.fold_left (fun m (s, t) -> StringMap.add s (t, false) m) StringMap.empty binds) in
    let (i', _) = (check_function_block_with_check (check_fstmt (UserDef c))) newScope funcs enums classes i in
    (binds, i')
  in

  let check_method scope funcs enums classes check_stmt f =
    let fname = f.fname in
    let params' = check_binds "formal" f.params in
    let newScope = WithParent(scope, List.fold_left (fun m (s, t) -> StringMap.add s (t, false) m) StringMap.empty params') in
    let (ss, _) = (check_function_block_with_check (check_stmt f.ty)) (WithParent(newScope, StringMap.empty)) funcs enums classes f.body in
    ({
      sfname = fname;
      sparams = params';
      sty = f.ty;
      sbody = List.rev ss;
    }, funcs) in

  let check_method_names ms =
    let compare m1 m2 = compare m1.fname m2.fname in
    let check_methods checked m =
      match checked with
        [] -> m :: checked
      | m' :: _ -> if m.fname = m'.fname then raise (Failure ("Class contains duplicate methods " ^ m.fname)) else m :: checked
    in List.fold_left check_methods [] (List.sort compare ms)
  in


  let check_cls scope funcs enums classes c =
    let cname = c.cname in
    let _ = if StringMap.mem cname classes then raise (Failure ("class " ^ cname ^ " already defined")) else () in
    let _ = if StringMap.mem cname enums then raise (Failure ("enum with name " ^ cname ^ " already defined")) else () in
    let fields = c.fields in
    let inits = c.inits in
    let methods = c.methods in

    let binds = check_binds "field" (List.map (fun (a, b, _) -> (a, b)) fields) in
    let fields' = List.map (expr scope funcs enums classes) (List.map (fun (_, _, c) -> c) fields) in
    let fields'' = List.map2 (fun (a, b) ((b', se) as e) -> if b = b' || se = SNoExpr then (a, b, e) else raise (Failure ("Bad field assign"))) binds fields' in
    let _ = check_inits_ps inits in
    let _ = check_method_names methods in
    let methodScope = List.fold_left (fun m (n, t, _) -> StringMap.add n (t, false) m) StringMap.empty fields'' in
    let newScope = WithParent(scope, StringMap.add "self" (UserDef cname, true) methodScope) in
    let newClasses = StringMap.add cname c classes in
    let inits' = List.map (check_init newScope funcs enums newClasses cname) inits in
    let methods' = List.map (fun m -> let (m', _) = check_method newScope funcs enums newClasses check_fstmt m in m') methods in
    ({ scname = cname ; sfields = fields'' ; sinits = inits' ; smethods = methods' }, newClasses)
  in





  let check_defn scope funcs enums classes = function
    Stmt s  -> let (s', newScope) = stmt scope funcs enums classes (check_toplevel_block_with_check stmt) s in (SStmt(s'), newScope, funcs, enums, classes)
  | Func_defn f ->
      let _ = if StringMap.mem f.fname funcs then raise (Failure ("Function " ^ f.fname ^ " already defined")) else () in
      let (f', newFuncs) = check_func scope funcs enums classes check_fstmt f in
      (SFunc_defn f', scope, newFuncs, enums, classes)
  | Enum_defn e -> let (e', newEnums) =  check_enum enums classes e in (SEnum_defn(e'), scope, funcs, newEnums, classes)
  | Cls_defn c -> let (c', newClasses) = check_cls scope funcs enums classes c in (SCls_defn(c'), scope, funcs, enums, newClasses)
  in
  let check_with_scope (newDefns, scope, funcs, enums, classes) d =
    let (d', newScope, newFuncs, newEnums, newClasses) = check_defn scope funcs enums classes d in
    (d'::newDefns, newScope, newFuncs, newEnums, newClasses) in
  let (defns', _, _, enums, classes) = List.fold_left check_with_scope ([], WithParent(Root, StringMap.empty), built_in_decls, StringMap.empty, StringMap.empty) defns
  in (List.rev defns', List.map (fun (a, _) -> a) (StringMap.bindings enums), List.map (fun (_, c) -> c) (StringMap.bindings classes))

