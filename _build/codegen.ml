module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

exception ToDo

type value_scope = Global of L.llvalue StringMap.t
                 | WithParent of value_scope * L.llvalue StringMap.t

let translate defns =
  let context = L.global_context () in
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context

  and the_module = L.create_module context "SwiftLite" in

(* Convert SwiftLite types to LLtype *)
   let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Char -> i8_t
    | A.String -> L.pointer_type i8_t
    | A.Float -> float_t
    | A.Optional t -> L.pointer_type (ltype_of_typ t)
    | A.UserDef _ -> raise ToDo
    | A.Array t -> L.pointer_type (L.struct_type context [| i32_t ; L.pointer_type (ltype_of_typ t) |])
  in 

  let print_t : L.lltype = 
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let print_func : L.llvalue = 
    L.declare_function "printf" print_t the_module in

  let toplevel = L.define_function "main" (L.function_type i32_t [||]) the_module in
  let builder = L.builder_at_end context (L.entry_block toplevel) in
  let string_format_string = L.build_global_stringptr "%s\n" "strfmt" builder in
  let int_format_string = L.build_global_stringptr "%d\n" "intfmt" builder in
  let char_format_string = L.build_global_stringptr "%c\n" "charfmt" builder in

  let lookup s scope =
    let rec findIn = function
        Global(map) -> StringMap.find s map
      | WithParent(p, map) -> try StringMap.find s map with Not_found -> findIn p
    in findIn scope in

  let add_terminal builder instr =
                           (* The current block where we're inserting instr *)
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

  let rec literal builder = function
      SIntLit i -> L.const_int i32_t i
    | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | SCharLit c -> L.const_int i8_t (Char.code c)
    | SFloatLit f ->  L.const_float float_t f
    | SStringLit str -> L.build_global_stringptr str "lit" builder
    | SArrayLit (t, ls) ->
      (* what do here? *)
        let t' = ltype_of_typ t in
        let str = L.build_malloc (L.struct_type context [| i32_t ; L.pointer_type t' |]) "arr" builder in
        let len = L.const_int i32_t (List.length ls) in
        let ls' = List.map (literal builder) ls in
        let arr = L.build_array_malloc t' len "temp" builder in
        let rec populate n = function
            [] -> ()
          | x::xs ->
              let gep = L.build_gep arr [| L.const_int i32_t n |] "tmp" builder in
              let _ = L.build_store x gep builder in
              populate (n + 1) xs
        in
        let () = populate 0 ls' in
        let len_gep = L.build_struct_gep str 0 "len" builder in
        let arr_gep = L.build_struct_gep str 1 "a" builder in
        let _ = L.build_store len len_gep builder in
        let _ = L.build_store arr arr_gep builder in
        str

    | SNil t -> L.const_null (L.pointer_type (ltype_of_typ t)) in

let rec expr builder scope funcs the_function ((_, e) : sexpr) = match e with
    SLiteral(l) -> literal builder l
  | SId(s) -> L.build_load (lookup s scope) s builder
  | SAssign(s, e) ->
      let e' = expr builder scope funcs the_function e in
      let _ = L.build_store e' (lookup s scope) builder in
      e'
  | SUnop(op, e) ->
      let (t, _) = e in
      let e' = expr builder scope funcs the_function e in
      (match op with
        A.Neg when t = A.Float -> L.build_fneg 
      | A.Neg                  -> L.build_neg
      | A.Not                  -> L.build_not) e' "tmp" builder

  | SBinop(e1, op, e2) ->
      let (t, _) = e1
      and e1' = expr builder scope funcs the_function e1
      and e2' = expr builder scope funcs the_function e2 in
      if t = A.Float then (match op with 
        A.Add     -> L.build_fadd
      | A.Sub     -> L.build_fsub
      | A.Mul    -> L.build_fmul
      | A.Div     -> L.build_fdiv 
      | A.Eq   -> L.build_fcmp L.Fcmp.Oeq
      | A.Neq     -> L.build_fcmp L.Fcmp.One
      | A.Lt    -> L.build_fcmp L.Fcmp.Olt
      | A.Leq     -> L.build_fcmp L.Fcmp.Ole
      | A.Gt -> L.build_fcmp L.Fcmp.Ogt
      | A.Geq     -> L.build_fcmp L.Fcmp.Oge
      | _ ->
          raise (Failure "internal error: semant should have rejected and/or on float")
      ) e1' e2' "tmp" builder 
      else if t = A.String then (match op with
        A.Add -> raise ToDo
            (*let concat e1 e2 s builder =
              let e1' = expr builder scope funcs e1
              and e2' = expr builder scope funcs e2 in
              let len1 = L.const_int (L.array_length (L.type_of e1'))
              and len2 = L.const_int (L.array_length (L.type_of e2')) in


            in concat*)
      | _ ->
          raise (Failure "internal error: semant should have rejected op on string")
      )
      else if (match t with A.Array _ -> true | _ -> false) then (match op with
        A.Eq   -> raise ToDo
      | A.Neq     -> raise ToDo
      | _ -> 
          raise (Failure "internal error: semant should have rejected op on Array")
      )
      else (match op with
      | A.Add     -> L.build_add
      | A.Sub     -> L.build_sub
      | A.Mul     -> L.build_mul
      | A.Div     -> L.build_sdiv
      | A.Mod     -> L.build_srem
      | A.And     -> L.build_and
      | A.Or      -> L.build_or
      | A.Eq      -> L.build_icmp L.Icmp.Eq
      | A.Neq     -> L.build_icmp L.Icmp.Ne
      | A.Lt      -> L.build_icmp L.Icmp.Slt
      | A.Leq     -> L.build_icmp L.Icmp.Sle
      | A.Gt      -> L.build_icmp L.Icmp.Sgt
      | A.Geq     -> L.build_icmp L.Icmp.Sge
      | A.Crange  -> raise ToDo(*
                (let closeRange e1 e2 s builder =
                  let len = L.build_sub (L.build_sub e2' e1' "tmp" builder) (L.const_int i32_t 1) "tmp" builder in
                  let arr = L.build_array_alloca i32_t len "tmp" builder in
                  let x = L.build_alloca i32_t "tmp" builder in
                  let i = L.build_alloca i32_t "tmp" builder in
                  let _ = L.build_store (L.const_int i32_t 0) x builder in
                  let _ = L.build_store (L.build_add x e1 "tmp" builder) i builder in
      
                  let pred_bb = L.append_block context "while" the_function in
                  let _ = L.build_br pred_bb builder in
                  
                  let body_bb = L.append_block context "while_body" the_function in
                  let body_builder = L.builder_at_end context body_bb in
                  let gep = L.build_gep arr [| x |] "tmp" body_builder in
                  let _ = L.build_store (L.build_add x e1 "tmp" body_builder) i body_builder in
                  let _ = L.build_store i gep body_builder in
                  let () = add_terminal body_builder (L.build_br pred_bb) in
      
                  let pred_builder = L.builder_at_end context pred_bb in
                  let pred = L.build_icmp L.Icmp.Slt  i e2 "tmp" builder in
      
                  let merge_bb = L.append_block context "merge" the_function in
                  let _ = L.build_cond_br pred body_bb merge_bb pred_builder in
                    
                  arr
                  
                in closeRange)*)
      | A.Orange  -> raise ToDo
      ) e1' e2' "tmp" builder
  | SArrAt(a, i) ->
      let a' = expr builder scope funcs the_function a in
      let i' = expr builder scope funcs the_function i in
      let arr_gep = L.build_struct_gep a' 1 "arr" builder in
      let arr = L.build_load arr_gep "arr" builder in
      let at_gep = L.build_gep arr [| i' |] "arrAt" builder in
      L.build_load at_gep "at" builder
  | SArrAssign(a, i, e) ->
      let a' = expr builder scope funcs the_function a in
      let i' = expr builder scope funcs the_function i in
      let e' = expr builder scope funcs the_function e in
      let arr_gep = L.build_struct_gep a' 1 "arr" builder in
      let arr = L.build_load arr_gep "arr" builder in
      let at_gep = L.build_gep arr [| i' |] "arrAt" builder in
      L.build_store e' at_gep builder
  | SCall("print", [e]) -> 
      let v = (expr builder scope funcs the_function e) in
       L.build_call print_func [| string_format_string; v |] "printf" builder
  | SCall("printi", [e]) | SCall("printb", [e]) ->
      let v = expr builder scope funcs the_function e in
       L.build_call print_func [| int_format_string; v |] "printf" builder
  | SCall("printc", [e]) ->
      let v = expr builder scope funcs the_function e in
       L.build_call print_func [| char_format_string; v |] "printf" builder
  | SCall(f, es) ->
      let fdef = StringMap.find f funcs in
      let es' = List.map (expr builder scope funcs the_function) es in
      L.build_call fdef (Array.of_list es') (f ^ "_result") builder
  | SOpt((t, _) as e) ->
      let e' = expr builder scope funcs the_function e in
      let opt = L.build_malloc (ltype_of_typ t) "opt" builder in
      let _ = L.build_store e' opt builder in
      opt
  in

  let rec stmt builder scope funcs the_function s =
    let build_stmt_in_block (builder, scope) s = stmt builder scope funcs the_function s in
    let add_global n t e builder =
      let global_var = L.define_global n (L.const_null (ltype_of_typ t)) the_module in
      let _ = L.build_store e global_var builder in
      global_var
    in
    match s with
        (* Generate code for this expression, return resulting builder *)
        SExpr e -> let _ = expr builder scope funcs the_function e in (builder, scope)
      | SVar (n, t, e) ->
          let e' = expr builder scope funcs the_function e in
          let newScope =
            match scope with 
              WithParent(p, map) ->
                let local_var = L.build_alloca (ltype_of_typ t) n builder in
                let _ = L.build_store e' local_var builder in
                  WithParent(p, StringMap.add n local_var map)
              | Global(map) ->
                let global_var = add_global n t e' builder in
                  Global(StringMap.add n global_var map) in
          (builder, newScope)
      | SLet (n, t, e) ->
          let e' = expr builder scope funcs the_function e in
          let newScope =
            match scope with 
              WithParent(p, map) ->
                let local_var = L.build_alloca (ltype_of_typ t) n builder in
                let _ = L.build_store e' local_var builder in
                  WithParent(p, StringMap.add n local_var map)
              | Global(map) ->
                let global_var = add_global n t e' builder in
                  Global(StringMap.add n global_var map) in
          (builder, newScope)
      | SIf (c, tblock, fblock) ->
          let c' = expr builder scope funcs the_function c in
          let merge_bb = L.append_block context "merge" the_function in
          let branch_instr = L.build_br merge_bb in

          let then_bb = L.append_block context "then" the_function in
          let (then_builder, _) = List.fold_left build_stmt_in_block (L.builder_at_end context then_bb, WithParent(scope, StringMap.empty)) tblock in
          let () = add_terminal then_builder branch_instr in

          let else_bb = L.append_block context "else" the_function in
          let (else_builder, _) = List.fold_left build_stmt_in_block (L.builder_at_end context else_bb, WithParent(scope, StringMap.empty)) fblock in
          let () = add_terminal else_builder branch_instr in

          let _ = L.build_cond_br c' then_bb else_bb builder in
          (L.builder_at_end context merge_bb, scope)
      | SIfLet (s, t, e, tblock, fblock) ->
          let e' = expr builder scope funcs the_function e in
          let merge_bb = L.append_block context "merge" the_function in
          let branch_instr = L.build_br merge_bb in

          let then_bb = L.append_block context "then" the_function in
          let then_builder = L.builder_at_end context then_bb in
          let s' = L.build_alloca (ltype_of_typ t) s then_builder in
          let e'' = L.build_load e' s then_builder in
          let _ = L.build_store e'' s' then_builder in
          let (then_builder, _) = List.fold_left build_stmt_in_block (then_builder, WithParent(scope, StringMap.add s s' StringMap.empty)) tblock in
          let () = add_terminal then_builder branch_instr in

          let else_bb = L.append_block context "else" the_function in
          let (else_builder, _) = List.fold_left build_stmt_in_block (L.builder_at_end context else_bb, WithParent(scope, StringMap.empty)) fblock in
          let () = add_terminal else_builder branch_instr in

          let cond = L.build_is_not_null e' "check" builder in
          let _ = L.build_cond_br cond then_bb else_bb builder in
          (L.builder_at_end context merge_bb, scope)
      | SWhile (c, body) ->
          let pred_bb = L.append_block context "while" the_function in
          let _ = L.build_br pred_bb builder in

          let body_bb = L.append_block context "while_body" the_function in
          let (while_builder, _) = List.fold_left build_stmt_in_block (L.builder_at_end context body_bb, WithParent(scope, StringMap.empty)) body in
          let () = add_terminal while_builder (L.build_br pred_bb) in

          let pred_builder = L.builder_at_end context pred_bb in
          let c' = expr pred_builder scope funcs the_function c in

          let merge_bb = L.append_block context "merge" the_function in
          let _ = L.build_cond_br c' body_bb merge_bb pred_builder in
          (L.builder_at_end context merge_bb, scope)
      | SFor (s, ((t, _) as e), body) ->
          let e' = expr builder scope funcs the_function e in
          let t' = ltype_of_typ (match t with A.Array t' -> t' | _ -> raise (Failure("Semantic check failed"))) in
          let s' = L.build_alloca t' s builder in

          let len_gep = L.build_struct_gep e' 0 "len" builder in
          let len = L.build_load len_gep "len" builder in
          let arr_gep = L.build_struct_gep e' 1 "arr" builder in
          let arr = L.build_load arr_gep "arr" builder in

          let count = L.build_alloca i32_t "count" builder in
          let _ = L.build_store (L.const_int i32_t 0) count builder in

          let pred_bb = L.append_block context "for" the_function in
          let _ = L.build_br pred_bb builder in

          let body_bb = L.append_block context "for_body" the_function in
          let for_builder = L.builder_at_end context body_bb in

          let i = L.build_load count "i" for_builder in
          let tmp_gep = L.build_gep arr [| i |] "tmp" for_builder in
          let _ = L.build_store (L.build_load tmp_gep "i" for_builder) s' for_builder in

          let (for_builder, _) = List.fold_left build_stmt_in_block (for_builder, WithParent(scope, StringMap.add s s' StringMap.empty)) body in
          let _ = L.build_store (L.build_add i (L.const_int  i32_t 1) "tmp" for_builder) count for_builder in
          let () = add_terminal for_builder (L.build_br pred_bb) in

          let pred_builder = L.builder_at_end context pred_bb in
          let check = L.build_icmp L.Icmp.Slt (L.build_load count "comp" pred_builder) len "check" pred_builder in

          let merge_bb = L.append_block context "merge" the_function in
          let _ = L.build_cond_br check body_bb merge_bb pred_builder in
          (L.builder_at_end context merge_bb, scope)
      | SReturn e ->
          let _ = L.build_ret (expr builder scope funcs the_function e) builder 
          in (builder, scope)
  in

  let build_defn builder scope funcs = function
    SStmt(s) -> let (b, s) = stmt builder scope funcs toplevel s in (b, s, funcs)
  | SFunc_defn(fdecl) ->
      let name = fdecl.sfname
      and formal_types = 
            Array.of_list (List.map (fun (_, t) -> ltype_of_typ t) fdecl.sparams)
      in let ftype = L.function_type (ltype_of_typ fdecl.sty) formal_types in
      let the_function = L.define_function name ftype the_module in
      let fbuilder = L.builder_at_end context (L.entry_block the_function) in
      let funcs' = StringMap.add name the_function funcs in

      let add_formal m (n, t) p = 
        let () = L.set_value_name n p in
        let local = L.build_alloca (ltype_of_typ t) n fbuilder in
        let _  = L.build_store p local fbuilder in
        StringMap.add n local m 
      in

      let formalScope = WithParent(scope, List.fold_left2 add_formal StringMap.empty fdecl.sparams (Array.to_list (L.params the_function))) in

      let (fbuilder, _) = List.fold_left (fun (b, s) st -> stmt b s funcs' the_function st) (fbuilder, WithParent(formalScope, StringMap.empty)) fdecl.sbody in
      let _ =
        add_terminal fbuilder (match fdecl.sty with
          A.Float -> L.build_ret (L.const_float float_t 0.0)
        | A.String -> L.build_ret (L.build_global_stringptr "" "ret" fbuilder)
        | A.Array t -> L.build_ret (L.build_array_alloca (ltype_of_typ t) (L.const_int i32_t 0) "ret" fbuilder)
        | A.Optional t -> L.build_ret (L.const_null (L.pointer_type (ltype_of_typ t)))
        | t -> L.build_ret (L.const_int (ltype_of_typ t) 0)) in
      (builder, scope, funcs')
      
 
  in
  let build_with_scope (builder, scope, funcs) d = build_defn builder scope funcs d in 
  let (builder, _, _) = List.fold_left build_with_scope (builder, Global(StringMap.empty), StringMap.empty) defns in
  ignore ((L.build_ret (L.const_int i32_t 0)) builder);
  the_module
