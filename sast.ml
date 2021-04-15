open Ast

exception ToDo

type slit =
  SIntLit of int
| SBoolLit of bool
| SCharLit of char
| SFloatLit of float
| SStringLit of string
| SArrayLit of slit list
| SNil

type sexpr = typ * sx
and sx =
    SLiteral of slit
  | SId of string
  | SAssign of string * sexpr
  | SCall of string * sexpr list

type sstmt =
  SExpr of sexpr
| SVar of string * typ * sexpr
| SLet of string * typ * sexpr

type sdefn = SStmt of sstmt

type sprogram = sdefn list


let string_of_sliteral = function
  SStringLit(s) -> "\"" ^ s ^ "\""
| _ -> raise ToDo

let rec string_of_sexpr (_, e) = match e with
  SLiteral(l) -> string_of_sliteral l
| SId(s) -> s
| SAssign(s, e) -> s ^ " = " ^ string_of_sexpr e
| SCall(f, es) -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr es) ^ ")"

let string_of_sstmt = function
  SExpr(e) -> string_of_sexpr e ^ ";\n"
| SVar(n, t, e) -> "var " ^ n ^ " : " ^ string_of_type t ^ " = " ^ string_of_sexpr e ^ ";"
| SLet(n, t, e) -> "let " ^ n ^ " : " ^ string_of_type t ^ " = " ^ string_of_sexpr e ^ ";"

let string_of_sdefn = function
  SStmt(s) -> string_of_sstmt s
let string_of_sprogram ds = 
  String.concat "\n" (List.map string_of_sdefn ds) ^ "\n"