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
  (*and array_t    = L.array_type *)

  and the_module = L.create_module context "SwiftLite" in

(* Convert SwiftLite types to LLtype *)
   let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Char -> i8_t
    | A.String -> L.pointer_type i8_t
    | A.Float -> float_t
    | A.Optional _ -> raise ToDo
    | A.UserDef _ -> raise ToDo
    | A.Array _ -> raise ToDo
  in 

  let print_t : L.lltype = 
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let print_func : L.llvalue = 
    L.declare_function "printf" print_t the_module in

  let toplevel = L.define_function "main" (L.function_type i32_t [||]) the_module in
  let builder = L.builder_at_end context (L.entry_block toplevel) in
  let string_format_string = L.build_global_stringptr "%s\n" "fmt" builder in

  let lookup s scope =
    let rec findIn = function
        Global(map) -> StringMap.find s map
      | WithParent(p, map) -> try StringMap.find s map with Not_found -> findIn p
    in findIn scope in

  let build_defn builder scope = function
    SStmt(s) ->
      let literal builder = function
        SIntLit i -> L.const_int i32_t i
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SCharLit _ -> raise ToDo
      | SFloatLit f ->  L.const_float float_t f
      | SStringLit str -> L.build_global_stringptr str "lit" builder
      | SArrayLit _ -> raise ToDo
      | SNil -> raise ToDo in

      let rec expr builder scope ((_, e) : sexpr) = match e with
        SLiteral(l) -> literal builder l
      | SId(s) -> L.build_load (lookup s scope) s builder
      | SAssign(s, e) ->
          let e' = expr builder scope e in
          let _ = L.build_store e' (lookup s scope) builder in
          e'
      | SCall("print", [e]) -> 
          let v = (expr builder scope e) in
          L.build_call print_func [| string_format_string; v |] "printf" builder
      | _ -> raise ToDo in
      

      let stmt builder scope = function
            (* Generate code for this expression, return resulting builder *)
            SExpr e -> let _ = expr builder scope e in (builder, scope)
          | SVar (n, t, e) ->
              let e' = expr builder scope e in
              let newScope =
                match scope with 
                  WithParent(p, map) ->
                    let local_var = L.build_alloca (ltype_of_typ t) n builder in
                    let _ = L.build_store e' local_var builder in
                      WithParent(p, StringMap.add n local_var map)
                  | Global(map) ->
                    let global_var = L.define_global n e' the_module in
                      Global(StringMap.add n global_var map) in
              (builder, newScope)
          | SLet (n, t, e) ->
              let e' = expr builder scope e in
              let newScope =
                match scope with 
                  WithParent(p, map) ->
                    let local_var = L.build_alloca (ltype_of_typ t) n builder in
                    let _ = L.build_store e' local_var builder in
                      WithParent(p, StringMap.add n local_var map)
                  | Global(map) ->
                    let global_var = L.define_global n e' the_module in
                    let _ = L.set_global_constant true global_var in
                      Global(StringMap.add n global_var map) in
              (builder, newScope)
          | _ -> raise ToDo
      in
      stmt builder scope s
 
  in
  let build_with_scope (builder, scope) d = build_defn builder scope d in 
  let (builder, _) = List.fold_left build_with_scope (builder, Global(StringMap.empty)) defns in
  ignore ((L.build_ret (L.const_int i32_t 0)) builder);
  the_module
