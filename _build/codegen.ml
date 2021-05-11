module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

exception ToDo

type value_scope = Global of L.llvalue StringMap.t
                 | WithParent of value_scope * L.llvalue StringMap.t

let translate (defns, enums, classes) =
  let context = L.global_context () in
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context in
  let string_t   = L.pointer_type (L.struct_type context [| i32_t ; L.pointer_type i8_t |])
  and array_t t  = L.pointer_type (L.struct_type context [| i32_t ; L.pointer_type t |])

  and the_module = L.create_module context "SwiftLite" in
  (*let get_cdefn s = List.find (fun c -> c.A.cname = s) classes in*)


(* Convert SwiftLite types to LLtype *)
 let rec ltype_of_typ map = function
    A.Int   -> i32_t
  | A.Bool  -> i1_t
  | A.Char -> i8_t
  | A.String -> string_t
  | A.Float -> float_t
  | A.Optional t -> L.pointer_type (ltype_of_typ map t)
  | A.UserDef s -> if List.mem s enums then i32_t else L.pointer_type (StringMap.find s map)
  | A.Array t -> L.pointer_type (L.struct_type context [| i32_t ; L.pointer_type (ltype_of_typ map t) |])
  in

  let class_structs =
    let build_named_struct map c =
      let struct_t = L.named_struct_type context c.A.cname
      in StringMap.add c.A.cname struct_t map in
    List.fold_left build_named_struct StringMap.empty classes
  in
 
  let build_struct map c =
    let struct_t = StringMap.find c.A.cname class_structs in
    let fields = c.A.fields in
    let ts = List.map (fun (_, b, _) -> b) fields in
    let ts' = List.map (ltype_of_typ class_structs) ts in
    let methods = c.A.methods in
    let mts = List.map (fun m -> 
      L.pointer_type (L.function_type (ltype_of_typ class_structs m.A.ty) 
        (Array.of_list (L.pointer_type struct_t :: (List.map (ltype_of_typ class_structs) 
          (List.map (fun (_, b) -> b) m.A.params)))))) methods in
    let () = L.struct_set_body struct_t (Array.of_list (ts' @ mts)) true in
  StringMap.add c.A.cname struct_t map 
  in
  let class_structs = List.fold_left build_struct StringMap.empty classes in


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


  (* STRING CONCATENATION - it's a long function *)

  let concat_func =
    L.define_function "concat" (L.function_type string_t [|string_t ; string_t|]) the_module in
  let concat_builder = L.builder_at_end context (L.entry_block concat_func) in
  let concat_params = L.params concat_func in
  let () = L.set_value_name "s1" (Array.get concat_params 0)  in
  let () = L.set_value_name "s2" (Array.get concat_params 1)  in

  let concat_str = L.build_malloc (L.struct_type context [| i32_t ; L.pointer_type i8_t |]) "cc" concat_builder in
  let concat_s1' = (Array.get concat_params 0) in
  let concat_s2' = (Array.get concat_params 1) in

  let concat_l1 = L.build_load (L.build_struct_gep concat_s1' 0 "len1" concat_builder) "len1" concat_builder in
  let concat_l2 = L.build_load (L.build_struct_gep concat_s2' 0 "len1" concat_builder) "len2" concat_builder in
  let concat_p1 = L.build_load (L.build_struct_gep concat_s1' 1 "p1" concat_builder) "p1" concat_builder in
  let concat_p2 = L.build_load (L.build_struct_gep concat_s2' 1 "p2" concat_builder) "p2" concat_builder in
  let concat_len = L.build_add concat_l1 concat_l2 "tmp" concat_builder in
  let concat_len_gep = L.build_struct_gep concat_str 0 "len" concat_builder in
  let _ = L.build_store concat_len concat_len_gep concat_builder in
  let concat_arr_gep = L.build_struct_gep concat_str 1 "chars" concat_builder in
  let concat_arr = L.build_array_malloc i8_t
    (L.build_add concat_len (L.const_int i32_t 1) "tmp" concat_builder) "arr" concat_builder in

  let concat_count = L.build_alloca i32_t "count" concat_builder in
  let _ = L.build_store (L.const_int i32_t 0) concat_count concat_builder in

  let concat_pred_bb1 = L.append_block context "for" concat_func in
  let _ = L.build_br concat_pred_bb1 concat_builder in

  let concat_body_bb = L.append_block context "for_body" concat_func in
  let concat_body_builder = L.builder_at_end context concat_body_bb in

  let concat_count' = L.build_load concat_count "count" concat_body_builder in
  let concat_at = L.build_gep concat_arr [| concat_count' |] "c" concat_body_builder in
  let concat_s1_at = L.build_gep concat_p1 [| concat_count' |] "c1" concat_body_builder in
  let _ = L.build_store (L.build_load concat_s1_at "tmp" concat_body_builder) concat_at concat_body_builder in
  let _ = L.build_store (L.build_add concat_count'
    (L.const_int i32_t 1) "tmp" concat_body_builder) concat_count concat_body_builder in
  let () = add_terminal concat_body_builder (L.build_br concat_pred_bb1) in

  let concat_pred_builder1 = L.builder_at_end context concat_pred_bb1 in
  let concat_check = L.build_icmp L.Icmp.Slt
    (L.build_load concat_count "comp" concat_pred_builder1) concat_l1 "check" concat_pred_builder1 in

  let concat_merge_bb = L.append_block context "merge" concat_func in
  let _ = L.build_cond_br concat_check concat_body_bb concat_merge_bb concat_pred_builder1 in

  let concat_builder' = L.builder_at_end context concat_merge_bb in
  let concat_count'' = L.build_alloca i32_t "count" concat_builder' in
  let _ = L.build_store (L.const_int i32_t 0) concat_count'' concat_builder' in

  let concat_pred_bb2 = L.append_block context "for" concat_func in
  let _ = L.build_br concat_pred_bb2 concat_builder' in

  let concat_body_bb' = L.append_block context "for_body" concat_func in
  let concat_body_builder' = L.builder_at_end context concat_body_bb' in

  let concat_count''' = L.build_load concat_count "count" concat_body_builder' in
  let concat_count'''' = L.build_load concat_count'' "count" concat_body_builder' in
  let concat_at' = L.build_gep concat_arr [| concat_count''' |] "c" concat_body_builder' in
  let concat_s1_at' = L.build_gep concat_p2 [| concat_count'''' |] "c1" concat_body_builder' in
  let _ = L.build_store (L.build_load concat_s1_at' "tmp" concat_body_builder')
    concat_at' concat_body_builder' in
  let _ = L.build_store (L.build_add concat_count''' (L.const_int i32_t 1) "tmp" concat_body_builder')
    concat_count concat_body_builder' in
  let _ = L.build_store (L.build_add concat_count'''' (L.const_int i32_t 1) "tmp" concat_body_builder')
    concat_count'' concat_body_builder' in
  let () = add_terminal concat_body_builder' (L.build_br concat_pred_bb2) in

  let concat_pred_builder2 = L.builder_at_end context concat_pred_bb2 in
  let concat_check' = L.build_icmp L.Icmp.Slt
    (L.build_load concat_count'' "comp" concat_pred_builder2) concat_l2 "check" concat_pred_builder2 in

  let concat_merge_bb' = L.append_block context "merge" concat_func in
  let _ = L.build_cond_br concat_check' concat_body_bb' concat_merge_bb' concat_pred_builder2 in

  let concat_builder'' = L.builder_at_end context concat_merge_bb' in
  let end_gep = L.build_gep concat_arr [| concat_len |] "end" concat_builder'' in
  let _ = L.build_store (L.const_int i8_t 0) end_gep concat_builder'' in
  let _ = L.build_store concat_arr concat_arr_gep concat_builder'' in
  let () = add_terminal concat_builder'' (L.build_ret concat_str) in

  (* END STRING CONCATENATION *)

  (* INTEGER RANGE FUNCTION - it's also long *)
  let range_func =
    L.define_function "range" (L.function_type (array_t i32_t) [| i32_t ; i32_t |]) the_module in
  let range_builder = L.builder_at_end context (L.entry_block range_func) in
  let range_params = L.params range_func in
  let () = L.set_value_name "s1" (Array.get range_params 0)  in
  let () = L.set_value_name "s2" (Array.get range_params 1)  in

  let range_str = L.build_malloc (L.struct_type context [| i32_t ; L.pointer_type i32_t |]) "arr" range_builder in
  let range_i = (Array.get range_params 0) in
  let range_j = (Array.get range_params 1) in

    let range_len = L.build_add (L.build_sub range_j range_i "tmp" range_builder)
      (L.const_int i32_t 1) "tmp" range_builder in
    let range_arr = L.build_array_malloc i32_t range_len "tmp" range_builder in
    let range_x = L.build_alloca i32_t "tmp" range_builder in
    let range_index = L.build_alloca i32_t "tmp" range_builder in
    let _ = L.build_store (L.const_int i32_t 0) range_index range_builder in
    let _ = L.build_store range_i range_x range_builder in

    let range_pred_bb = L.append_block context "while" range_func in
    let _ = L.build_br range_pred_bb range_builder in
    
    let range_body_bb = L.append_block context "while_body" range_func in
    let range_body_builder = L.builder_at_end context range_body_bb in
    let range_index' = L.build_load range_index "i" range_body_builder in
    let range_x' = L.build_load range_x "x" range_body_builder in
    let range_gep = L.build_gep range_arr [| range_index' |] "tmp" range_body_builder in
    let _ = L.build_store range_x' range_gep range_body_builder in
    let _ = L.build_store (L.build_add range_x' (L.const_int i32_t 1) "tmp" range_body_builder)
      range_x range_body_builder in
    let _ = L.build_store (L.build_add range_index' (L.const_int i32_t 1) "tmp" range_body_builder)
      range_index range_body_builder in
    let () = add_terminal range_body_builder (L.build_br range_pred_bb) in

    let range_pred_builder = L.builder_at_end context range_pred_bb in
    let range_pred = L.build_icmp L.Icmp.Slt (L.build_load range_index "i" range_pred_builder)
      range_len "tmp" range_pred_builder in

    let range_merge_bb = L.append_block context "merge" range_func in
    let _ = L.build_cond_br range_pred range_body_bb range_merge_bb range_pred_builder in
    
  let range_merge_builder = L.builder_at_end context range_merge_bb in  
  let range_len_gep = L.build_struct_gep range_str 0 "len_gep" range_merge_builder in
  let range_arr_gep = L.build_struct_gep range_str 1 "arr_gep" range_merge_builder in
  let _ = L.build_store range_len range_len_gep range_merge_builder in
  let _ = L.build_store range_arr range_arr_gep range_merge_builder in

  let () = add_terminal range_merge_builder (L.build_ret range_str) in
  (* END INTEGER RANGES *)

  let rec literal builder = function
      SIntLit i -> L.const_int i32_t i
    | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
    | SCharLit c -> L.const_int i8_t (Char.code c)
    | SFloatLit f ->  L.const_float float_t f
    | SStringLit s -> 
        let str = L.build_malloc (L.struct_type context [| i32_t ; L.pointer_type i8_t |]) "arr" builder in
        let s' = L.build_global_stringptr s "lit" builder in
        let len = L.const_int i32_t (String.length s) in
        let len_gep = L.build_struct_gep str 0 "len" builder in
        let s_gep = L.build_struct_gep str 1 "a" builder in
        let _ = L.build_store len len_gep builder in
        let _ = L.build_store s' s_gep builder in
        str


    | SArrayLit (t, ls) ->
        let t' = ltype_of_typ class_structs t in
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

    | SNil t -> L.const_null (L.pointer_type (ltype_of_typ class_structs t)) in

let rec expr builder scope funcs enums classes the_function ((t, e) : sexpr) = match e with
    SLiteral(l) -> literal builder l
  | SId(s) -> L.build_load (lookup s scope) s builder
  | SAssign(s, e) ->
      let e' = expr builder scope funcs enums classes the_function e in
      let _ = L.build_store e' (lookup s scope) builder in
      e'
  | SUnop(op, e) ->
      let (t, _) = e in
      let e' = expr builder scope funcs enums classes the_function e in
      (match op with
        A.Neg when t = A.Float -> L.build_fneg 
      | A.Neg                  -> L.build_neg
      | A.Not                  -> L.build_not) e' "tmp" builder

  | SBinop(e1, op, e2) ->
      let (t, _) = e1
      and e1' = expr builder scope funcs enums classes the_function e1
      and e2' = expr builder scope funcs enums classes the_function e2 in
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
          raise (Failure "internal error: semant should have rejected op on float")
      ) e1' e2' "tmp" builder 
      else if t = A.String then (match op with
        A.Add -> L.build_call concat_func [| e1' ; e2' |] "concat" builder
      | _ ->
          raise (Failure "internal error: semant should have rejected op on string")
      )
      else (match op with
      | A.Add     -> L.build_add
      | A.Sub     -> L.build_sub
      | A.Mul     -> L.build_mul
      | A.Div     -> L.build_sdiv
      | A.Mod     -> L.build_urem
      | A.And     -> L.build_and
      | A.Or      -> L.build_or
      | A.Eq      -> L.build_icmp L.Icmp.Eq
      | A.Neq     -> L.build_icmp L.Icmp.Ne
      | A.Lt      -> L.build_icmp L.Icmp.Slt
      | A.Leq     -> L.build_icmp L.Icmp.Sle
      | A.Gt      -> L.build_icmp L.Icmp.Sgt
      | A.Geq     -> L.build_icmp L.Icmp.Sge
      | A.Crange  ->
          let range e1 e2 s builder =
            L.build_call range_func [| e1 ; (L.build_sub e2 (L.const_int i32_t 1) "tmp" builder) |] s builder
          in range
            
      | A.Orange  ->
          let range e1 e2 s builder =
            L.build_call range_func [| e1 ; e2 |] s builder
          in range
      ) e1' e2' "tmp" builder
  | SArrAt(a, i) ->
      let a' = expr builder scope funcs enums classes the_function a in
      let i' = expr builder scope funcs enums classes the_function i in
      let arr_gep = L.build_struct_gep a' 1 "arr" builder in
      let arr = L.build_load arr_gep "arr" builder in
      let at_gep = L.build_gep arr [| i' |] "arrAt" builder in
      L.build_load at_gep "at" builder
  | SArrAssign(a, i, e) ->
      let a' = expr builder scope funcs enums classes the_function a in
      let i' = expr builder scope funcs enums classes the_function i in
      let e' = expr builder scope funcs enums classes the_function e in
      let arr_gep = L.build_struct_gep a' 1 "arr" builder in
      let arr = L.build_load arr_gep "arr" builder in
      let at_gep = L.build_gep arr [| i' |] "arrAt" builder in
      L.build_store e' at_gep builder
  | SCall("print", [e]) -> 
      let v = (expr builder scope funcs enums classes the_function e) in
      let s = L.build_struct_gep v 1 "str" builder in
      let s' = L.build_load s "s" builder in
       L.build_call print_func [| string_format_string; s' |] "printf" builder
  | SCall("printi", [e]) | SCall("printb", [e]) ->
      let v = expr builder scope funcs enums classes the_function e in
       L.build_call print_func [| int_format_string; v |] "printf" builder
  | SCall("printc", [e]) ->
      let v = expr builder scope funcs enums classes the_function e in
       L.build_call print_func [| char_format_string; v |] "printf" builder
  | SCall(f, es) ->
      let fdef = StringMap.find f funcs in
      let es' = List.map (expr builder scope funcs enums classes the_function) es in
      L.build_call fdef (Array.of_list es') (f ^ "_result") builder
  | SOpt((t, _) as e) ->
      let e' = expr builder scope funcs enums classes the_function e in
      let opt = L.build_malloc (ltype_of_typ class_structs t) "opt" builder in
      let _ = L.build_store e' opt builder in
      opt
  | SEnumCase(s1, s2) ->
      let caseMap = StringMap.find s1 enums in
      StringMap.find s2 caseMap
  | SInit(s, s', es) -> 
      let (inits, _) = StringMap.find s classes in
      let init = StringMap.find s' inits in
      let es' = List.map (expr builder scope funcs enums classes the_function) es in
      L.build_call init (Array.of_list es') s' builder


      
  | SMethodCall(s1, s2, offset, es) ->
      let s1p = lookup s1 scope in
      let s1' = L.build_load s1p "obj" builder in
      let es' = List.map (expr builder scope funcs enums classes the_function) es in
      let m = L.build_load (L.build_struct_gep s1' offset "method" builder) s2 builder in
      L.build_call m (Array.of_list (s1'::es')) "call" builder
  | SSelf ->
      let selfp = lookup "self" scope in
      L.build_load selfp "self" builder
  | SSelfCall(s, offset, es) ->
      let selfp = lookup "self" scope in
      let self = L.build_load selfp "self" builder in
      let es' = List.map (expr builder scope funcs enums classes the_function) es in
      let m = L.build_load (L.build_struct_gep self offset "method" builder) s builder in
      L.build_call m (Array.of_list (self::es')) "call" builder
  | SSelfField(s, offset) ->
      let selfp = lookup "self" scope in
      let self = L.build_load selfp "self" builder in
      let f = L.build_load (L.build_struct_gep self offset s builder) s builder in
      f
  | SSelfAssign(s, offset, e) ->
      let e' = expr builder scope funcs enums classes the_function e in
      let selfp = lookup "self" scope in
      let self = L.build_load selfp "self" builder in
      let f = L.build_struct_gep self offset s builder in
      let _ = L.build_store e' f builder in
      e'
  | SNoExpr ->
      L.const_null (ltype_of_typ class_structs t)
  in

  let rec stmt builder scope funcs enums classes the_function s =
    let build_stmt_in_block (builder, scope) s = stmt builder scope funcs enums classes the_function s in
    let add_global n t e builder =
      let global_var = L.define_global n (L.const_null (ltype_of_typ class_structs t)) the_module in
      let _ = L.build_store e global_var builder in
      global_var
    in
    match s with
        (* Generate code for this expression, return resulting builder *)
        SExpr e -> let _ = expr builder scope funcs enums classes the_function e in (builder, scope)
      | SVar (n, t, e) ->
          let e' = expr builder scope funcs enums classes the_function e in
          let newScope =
            match scope with 
              WithParent(p, map) ->
                let local_var = L.build_alloca (ltype_of_typ class_structs t) n builder in
                let _ = L.build_store e' local_var builder in
                  WithParent(p, StringMap.add n local_var map)
              | Global(map) ->
                let global_var = add_global n t e' builder in
                  Global(StringMap.add n global_var map) in
          (builder, newScope)
      | SLet (n, t, e) ->
          let e' = expr builder scope funcs enums classes the_function e in
          let newScope =
            match scope with 
              WithParent(p, map) ->
                let local_var = L.build_alloca (ltype_of_typ class_structs t) n builder in
                let _ = L.build_store e' local_var builder in
                  WithParent(p, StringMap.add n local_var map)
              | Global(map) ->
                let global_var = add_global n t e' builder in
                  Global(StringMap.add n global_var map) in
          (builder, newScope)
      | SIf (c, tblock, fblock) ->
          let c' = expr builder scope funcs enums classes the_function c in
          let merge_bb = L.append_block context "merge" the_function in
          let branch_instr = L.build_br merge_bb in

          let then_bb = L.append_block context "then" the_function in
          let (then_builder, _) = List.fold_left build_stmt_in_block
            (L.builder_at_end context then_bb, WithParent(scope, StringMap.empty)) tblock in
          let () = add_terminal then_builder branch_instr in

          let else_bb = L.append_block context "else" the_function in
          let (else_builder, _) = List.fold_left build_stmt_in_block
            (L.builder_at_end context else_bb, WithParent(scope, StringMap.empty)) fblock in
          let () = add_terminal else_builder branch_instr in

          let _ = L.build_cond_br c' then_bb else_bb builder in
          (L.builder_at_end context merge_bb, scope)
      | SIfLet (s, t, e, tblock, fblock) ->
          let e' = expr builder scope funcs enums classes the_function e in
          let merge_bb = L.append_block context "merge" the_function in
          let branch_instr = L.build_br merge_bb in

          let then_bb = L.append_block context "then" the_function in
          let then_builder = L.builder_at_end context then_bb in
          let s' = L.build_alloca (ltype_of_typ class_structs t) s then_builder in
          let e'' = L.build_load e' s then_builder in
          let _ = L.build_store e'' s' then_builder in
          let (then_builder, _) = List.fold_left build_stmt_in_block
            (then_builder, WithParent(scope, StringMap.add s s' StringMap.empty)) tblock in
          let () = add_terminal then_builder branch_instr in

          let else_bb = L.append_block context "else" the_function in
          let (else_builder, _) = List.fold_left build_stmt_in_block
            (L.builder_at_end context else_bb, WithParent(scope, StringMap.empty)) fblock in
          let () = add_terminal else_builder branch_instr in

          let cond = L.build_is_not_null e' "check" builder in
          let _ = L.build_cond_br cond then_bb else_bb builder in
          (L.builder_at_end context merge_bb, scope)
      | SWhile (c, body) ->
          let pred_bb = L.append_block context "while" the_function in
          let _ = L.build_br pred_bb builder in

          let body_bb = L.append_block context "while_body" the_function in
          let (while_builder, _) = List.fold_left build_stmt_in_block
            (L.builder_at_end context body_bb, WithParent(scope, StringMap.empty)) body in
          let () = add_terminal while_builder (L.build_br pred_bb) in

          let pred_builder = L.builder_at_end context pred_bb in
          let c' = expr pred_builder scope funcs enums classes the_function c in

          let merge_bb = L.append_block context "merge" the_function in
          let _ = L.build_cond_br c' body_bb merge_bb pred_builder in
          (L.builder_at_end context merge_bb, scope)
      | SFor (s, ((t, _) as e), body) ->
          let e' = expr builder scope funcs enums classes the_function e in
          let t' = ltype_of_typ class_structs
            (match t with A.Array t' -> t' | _ -> raise (Failure("Semantic check failed"))) in
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

          let (for_builder, _) = List.fold_left build_stmt_in_block
            (for_builder, WithParent(scope, StringMap.add s s' StringMap.empty)) body in
          let _ = L.build_store (L.build_add i (L.const_int  i32_t 1) "tmp" for_builder) count for_builder in
          let () = add_terminal for_builder (L.build_br pred_bb) in

          let pred_builder = L.builder_at_end context pred_bb in
          let check = L.build_icmp L.Icmp.Slt (L.build_load count "comp" pred_builder) len "check" pred_builder in

          let merge_bb = L.append_block context "merge" the_function in
          let _ = L.build_cond_br check body_bb merge_bb pred_builder in
          (L.builder_at_end context merge_bb, scope)
      | SReturn e ->
          let _ = L.build_ret (expr builder scope funcs enums classes the_function e) builder 
          in (builder, scope)
  in

  let build_defn builder scope funcs enums classes = function
    SStmt(s) -> let (b, s) = stmt builder scope funcs enums classes toplevel s in (b, s, funcs, enums, classes)
  | SFunc_defn(fdecl) ->
      let name = fdecl.sfname
      and formal_types = 
            Array.of_list (List.map (fun (_, t) -> ltype_of_typ class_structs t) fdecl.sparams)
      in let ftype = L.function_type (ltype_of_typ class_structs fdecl.sty) formal_types in
      let the_function = L.define_function name ftype the_module in
      let fbuilder = L.builder_at_end context (L.entry_block the_function) in
      let funcs' = StringMap.add name the_function funcs in

      let add_formal m (n, t) p = 
        let () = L.set_value_name n p in
        let local = L.build_alloca (ltype_of_typ class_structs t) n fbuilder in
        let _  = L.build_store p local fbuilder in
        StringMap.add n local m 
      in

      let formalScope = WithParent(scope, List.fold_left2 add_formal StringMap.empty fdecl.sparams
        (Array.to_list (L.params the_function))) in

      let (fbuilder, _) = List.fold_left (fun (b, s) st -> stmt b s funcs' enums classes the_function st)
        (fbuilder, WithParent(formalScope, StringMap.empty)) fdecl.sbody in
      let _ =
        add_terminal fbuilder (match fdecl.sty with
          A.Float -> L.build_ret (L.const_float float_t 0.0)
        | A.String -> L.build_ret (L.build_global_stringptr "" "ret" fbuilder)
        | A.Optional t -> L.build_ret (L.const_null (L.pointer_type (ltype_of_typ class_structs t)))
        | t -> L.build_ret (L.const_null (ltype_of_typ class_structs t))) in
      (builder, scope, funcs', enums, classes)
  | SEnum_defn edefn ->
      let name = edefn.sename
      and cases = edefn.scases in
      let nums = List.mapi (fun i _ -> i) cases in
      let cases' = List.fold_left2 (fun a s i -> StringMap.add s (L.const_int i32_t i) a)
        StringMap.empty cases nums in

      (builder, scope, funcs, StringMap.add name cases' enums, classes)
  | SCls_defn cdefn ->
      let cname = cdefn.scname in
      let fields = cdefn.sfields in
      let fieldOffsets = List.mapi (fun i (s, _, _) -> (i, s)) fields in
      let methods = cdefn.smethods in
      let inits = cdefn.sinits in
      let selfTy = ltype_of_typ class_structs (A.UserDef cname) in

      let funcs' = 
        let add_method funcs m =
          let name = m.sfname
          and formal_types = 
                Array.of_list (selfTy :: (List.map (fun (_, t) -> ltype_of_typ class_structs t) m.sparams))
          in let ftype = L.function_type (ltype_of_typ class_structs m.sty) formal_types in
          let the_function = L.define_function name ftype the_module in
          StringMap.add name the_function funcs
      in List.fold_left add_method funcs methods in

      let funcs' =
        let add_init funcs (i, (params, _)) =
          let name = cname ^ (string_of_int i)
          and formal_types =
                Array.of_list (List.map (fun (_, t) -> ltype_of_typ class_structs t) params) in
          let ftype = L.function_type selfTy formal_types in
          let the_function = L.define_function name ftype the_module in
          StringMap.add name the_function funcs
        in List.fold_left add_init funcs' (List.mapi (fun i a -> (i, a)) inits) in

      let methods' = 
        let build_method m =
          let name = m.sfname in

          let the_function = StringMap.find name funcs' in
          let fbuilder = L.builder_at_end context (L.entry_block the_function) in

          let add_formal m (n, t) p = 
            let () = L.set_value_name n p in
            let local = L.build_alloca (ltype_of_typ class_structs t) n fbuilder in
            let _  = L.build_store p local fbuilder in
            StringMap.add n local m 
          in

          let args = List.fold_left2 add_formal StringMap.empty
            (("self", A.UserDef cname) :: m.sparams) (Array.to_list (L.params the_function)) in

          let fieldScope =
            WithParent(scope, List.fold_left (fun map (i, n) ->
                let self = L.build_load (StringMap.find "self" args) "self" fbuilder in
                let f = L.build_struct_gep self i "f" fbuilder in
                StringMap.add n f map) StringMap.empty fieldOffsets) in




          let formalScope = WithParent(fieldScope, args) in

          let (fbuilder, _) = List.fold_left (fun (b, s) st -> stmt b s funcs' enums classes the_function st)
            (fbuilder, WithParent(formalScope, StringMap.empty)) m.sbody in
          let _ =
            add_terminal fbuilder (match m.sty with
              A.Float -> L.build_ret (L.const_float float_t 0.0)
            | A.String -> L.build_ret (L.build_global_stringptr "" "ret" fbuilder)
            | A.Optional t -> L.build_ret (L.const_null (L.pointer_type (ltype_of_typ class_structs t)))
            | t -> L.build_ret (L.const_null (ltype_of_typ class_structs t))) in
        the_function in List.map build_method methods
      in
      let inits' =
        let build_init map (x, (params, body)) =
          let name = cname ^ (string_of_int x) in
          let the_function = StringMap.find name funcs' in
          let fbuilder = L.builder_at_end context (L.entry_block the_function) in

          let add_formal m (n, t) p = 
            let () = L.set_value_name n p in
            let local = L.build_alloca (ltype_of_typ class_structs t) n fbuilder in
            let _  = L.build_store p local fbuilder in
            StringMap.add n local m 
          in

          let args = List.fold_left2 add_formal StringMap.empty params (Array.to_list (L.params the_function)) in

          let fields' = List.map (fun (_, t, (_, e)) ->
              expr fbuilder scope funcs enums classes toplevel (t, e)) fields in

          let t = StringMap.find cname class_structs in
          let obj = L.build_malloc t "obj" fbuilder in
          let init_field i f =
            let gep = L.build_struct_gep obj i "f" fbuilder in
            let _ = L.build_store f gep fbuilder in
            () in
          let _ = List.iteri init_field fields' in
          let num_fields = List.length fields' in
          let init_method i m =
            let gep = L.build_struct_gep obj (i + num_fields) "m" fbuilder in
            let _ = L.build_store m gep fbuilder in
            () in
          let _ = List.iteri init_method methods' in
          let self = L.build_malloc selfTy "self" fbuilder in
          let _ = L.build_store obj self fbuilder in

          let fieldScope =
            WithParent(scope, List.fold_left (fun map (i, n) ->
                let f = L.build_struct_gep obj i "f" fbuilder in
                StringMap.add n f map) StringMap.empty fieldOffsets) in

          let selfScope = WithParent(fieldScope, StringMap.add "self" self StringMap.empty) in


          let formalScope = WithParent(selfScope, args) in

          let (fbuilder, _) = List.fold_left (fun (b, s) st -> stmt b s funcs enums classes the_function st)
            (fbuilder, formalScope) body in
          let _ =
            add_terminal fbuilder (L.build_ret obj)in
          StringMap.add name the_function map
        in List.fold_left build_init StringMap.empty (List.mapi (fun i a -> (i, a)) inits)
      in
      (builder, scope, funcs, enums, StringMap.add cname (inits', methods') classes)
      
 
  in
  let build_with_scope (builder, scope, funcs, enums, classes) d =
    build_defn builder scope funcs enums classes d in 
  let (builder, _, _, _, _) =
    List.fold_left build_with_scope
    (builder, Global(StringMap.empty), StringMap.empty, StringMap.empty, StringMap.empty)
    defns
  in
  ignore ((L.build_ret (L.const_int i32_t 0)) builder);
  the_module
