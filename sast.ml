open Ast

exception ToDo

module StringMap = Map.Make(String)

type slit =
  SIntLit of int
| SBoolLit of bool
| SCharLit of char
| SFloatLit of float
| SStringLit of string
| SArrayLit of typ * slit list
| SNil of typ

type sexpr = typ * sx
and sx =
  SLiteral of slit
| SId of string
| SAssign of string * sexpr
| SBinop of sexpr * op * sexpr
| SUnop of uop * sexpr
| SArrAt of sexpr * sexpr
| SArrAssign of sexpr * sexpr * sexpr
| SCall of string * sexpr list
| SOpt of sexpr
| SEnumCase of string * string
| SMethodCall of string * string * int * sexpr list
| SInit of string * string * sexpr list
| SSelf
| SSelfCall of string * int * sexpr list
| SSelfField of string * int
| SSelfAssign of string * int * sexpr
| SNoExpr

type sstmt =
  SExpr of sexpr
| SVar of string * typ * sexpr
| SReturn of sexpr
| SLet of string * typ * sexpr
| SIf of sexpr * sblock * sblock
| SIfLet of string * typ * sexpr * sblock * sblock
| SWhile of sexpr * sblock
| SFor of string * sexpr * sblock
and sblock = sstmt list

type sfunc_defn = {
  sfname : string;
  sparams : (string * typ) list;
  sty : typ;
  sbody : sstmt list;
}

type senum_defn = {
  sename : string;
  scases : string list;
}

type scls_defn = {
  scname : string;
  sfields : (string * typ * sexpr) list;
  sinits : ((string * typ) list * sstmt list) list;
  smethods : sfunc_defn list;
}

type sdefn = SStmt of sstmt | SFunc_defn of sfunc_defn | SEnum_defn of senum_defn | SCls_defn of scls_defn

type sprogram = sdefn list * string list * cls_defn list


let rec string_of_sliteral = function
  SStringLit s -> "\"" ^ s ^ "\""
| SIntLit i -> string_of_int i
| SBoolLit b -> string_of_bool b
| SCharLit c -> Char.escaped c
| SFloatLit f -> string_of_float f
| SArrayLit (t, ls) -> string_of_type t ^ " [" ^ String.concat ", " (List.map string_of_sliteral ls) ^ "]"
| SNil(t) -> "nil : " ^ string_of_type t

let rec string_of_sexpr (_, e) = match e with
  SLiteral(l) -> string_of_sliteral l
| SId(s) -> s
| SAssign(s, e) -> s ^ " = " ^ string_of_sexpr e
| SBinop (e1, op, e2) -> string_of_sexpr e1 ^ " " ^ string_of_op op ^ " " ^ string_of_sexpr e2
| SUnop (op, e) -> string_of_uop op ^ " " ^ string_of_sexpr e
| SArrAt (a, i) -> string_of_sexpr a ^ "[" ^ string_of_sexpr i ^ "]"
| SArrAssign (a, i, e) -> string_of_sexpr a ^ "[" ^ string_of_sexpr i ^ "] = " ^ string_of_sexpr e
| SCall(f, es) -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr es) ^ ")"
| SMethodCall(s, s', _, es) -> s ^ "." ^ s' ^ "(" ^ String.concat ", " (List.map string_of_sexpr es) ^ ")"
| SOpt(e) -> string_of_sexpr e ^ "?"
| SEnumCase(s, s') -> s ^ "." ^ s'
| SInit(s, _, es) -> s ^ "(" ^ String.concat ", " (List.map string_of_sexpr es) ^ ")"
| SSelf -> "self"
| SSelfField(s, _) -> "self." ^ s
| SSelfCall(s, _, es) -> "self." ^ s ^ "(" ^ String.concat ", " (List.map string_of_sexpr es) ^ ")"
| SSelfAssign(s, _, e) -> "self." ^ s ^ " = " ^ string_of_sexpr e
| SNoExpr -> ""

let rec string_of_sstmt = function
  SExpr(e) -> string_of_sexpr e ^ ";\n"
| SVar(n, t, e) -> "var " ^ n ^ " : " ^ string_of_type t ^ " = " ^ string_of_sexpr e ^ ";"
| SReturn (e) -> "return " ^ string_of_sexpr e ^ ";"
| SLet(n, t, e) -> "let " ^ n ^ " : " ^ string_of_type t ^ " = " ^ string_of_sexpr e ^ ";"
| SIf(c, t, f) -> let fString = match f with [] -> "" | _ -> "else {\n" ^ String.concat "\n" (List.map string_of_sstmt f) ^ "\n}" in
      "if " ^ string_of_sexpr c ^ " {\n" ^
      String.concat "\n" (List.map string_of_sstmt t) ^ "\n}" ^ fString
| SIfLet(s, t, e, ts, fs) -> "if let " ^ s ^ ": " ^ string_of_type t ^ " = " ^ string_of_sexpr e ^ "{\n" ^
    String.concat "\n" (List.map string_of_sstmt ts) ^ "\n} else {\n" ^ String.concat "\n" (List.map string_of_sstmt fs) ^ "\n}"
| SWhile(p, b) -> "while " ^ string_of_sexpr p ^ "{\n" ^ String.concat "\n" (List.map string_of_sstmt b) ^ "\n}"
| SFor(s, e, ls) -> "for " ^ s ^ " in " ^ string_of_sexpr e ^ " {\n" ^ String.concat "\n" (List.map string_of_sstmt ls) ^ "\n}"

let string_of_sinit = function
  (xs, ys) -> "init(" ^ String.concat ", " (List.map string_of_arg xs) ^ ") {\n" ^ String.concat "\n" (List.map string_of_sstmt ys) ^ "\n}"

let string_of_sfunc f = 
  "func " ^ f.sfname ^ "(" ^ String.concat ", " (List.map string_of_arg f.sparams) ^ ") -> " ^ string_of_type f.sty ^ "{\n"
  ^ String.concat "\n" (List.map string_of_sstmt f.sbody) ^ "\n}"

let string_of_senum e =
  "enum " ^ e.sename ^ "{\n" ^ String.concat ";\n" (List.map (fun (s) -> "case " ^ s) e.scases) ^ "}\n"

let string_of_scls c =
  "class " ^ c.scname ^ " {\n" ^ String.concat "\n" (List.map (fun (s, t, e) -> string_of_sstmt (SVar (s, t, e))) c.sfields) ^
  String.concat "\n" (List.map string_of_sinit c.sinits)^ "\n" ^ String.concat "\n" (List.map string_of_sfunc c.smethods) ^ "\n}"


let string_of_sdefn = function
  SStmt(s) -> string_of_sstmt s
| SFunc_defn(f) -> string_of_sfunc f
| SEnum_defn e -> string_of_senum e
| SCls_defn _ -> "class\n"


let string_of_sprogram (ds, _, _) = 
  String.concat "\n" (List.map string_of_sdefn ds) ^ "\n"