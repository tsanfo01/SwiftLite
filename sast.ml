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
  | SCall of string * sexpr list

type sstmt =
  SExpr of sexpr

type sdefn = SStmt of sstmt

type sprogram = sdefn list


let string_of_sliteral = function
  SStringLit(s) -> s
| _ -> raise ToDo

let rec string_of_sexpr (_, e) = match e with
  SLiteral(l) -> string_of_sliteral l
| SCall(f, es) -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr es) ^ ")"

let string_of_sstmt = function
  SExpr(e) -> string_of_sexpr e ^ ";\n"

let string_of_sdefn = function
  SStmt(s) -> string_of_sstmt s
let string_of_sprogram ds = 
  String.concat "" (List.map string_of_sdefn ds) ^ "\n"