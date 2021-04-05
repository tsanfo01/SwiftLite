module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

exception ToDo

let translate defns =
  let context = L.global_context () in
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  (*and array_t    = L.array_type *)

  and the_module = L.create_module context "SwiftLite" in
(*
  let add_terminal builder instr =
                           (* The current block where we're inserting instr *)
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (instr builder) in *)

(* Convert SwiftLite types to LLtype *)
   (*let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Char -> i8_t
    | A.String -> L.pointer_type i8_t
    | A.Float -> float_t
    | A.Optional _ -> raise ToDo
    | A.UserDef _ -> raise ToDo
    | A.Array _ -> raise ToDo
  in  *)

  let print_t : L.lltype = 
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let print_func : L.llvalue = 
    L.declare_function "print" print_t the_module in

  let toplevel = L.define_function "main" i32_t the_module in
  let builder = L.builder_at_end context (L.entry_block toplevel) in

  let build_defn = function
    SStmt(s) ->
      let literal builder = function
        SIntLit i -> L.const_int i32_t i
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SCharLit _ -> raise ToDo
      | SFloatLit f ->  L.const_float float_t f
      | SStringLit str -> L.build_global_stringptr str "lit" builder
      | SArrayLit _ -> raise ToDo
      | SNil -> raise ToDo in

      let rec expr builder ((_, e) : sexpr) = match e with
        SLiteral(l) -> literal builder l
      | SCall("print", [e]) -> 
          L.build_call print_func [| (expr builder e) |] "print" builder
      | _ -> raise ToDo in
      

      let stmt builder = function
            (* Generate code for this expression, return resulting builder *)
            SExpr e -> let _ = expr builder e in builder 
      in
      ignore (stmt builder s)
 
  in List.iter build_defn defns;
  ignore ((L.build_ret (L.const_int i32_t 0)) (L.builder_at_end context (L.entry_block toplevel)));
  the_module
