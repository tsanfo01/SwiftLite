type typ = Int | Float | Char | String | Bool | Optional of typ
         | UserDef of string | Array of typ

type op = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Gt
        | Leq | Geq | And | Or | Orange | Crange
type uop = Not | Neg

type lit =
  IntLit of int
| BoolLit of bool
| CharLit of char
| FloatLit of float
| StringLit of string
| ArrayLit of lit list
| Nil


type expr =
  Literal of lit
| Id of string
| Binop of expr * op * expr
| Unop of uop * expr
| Assign of string * expr
| ArrAssign of expr * expr * expr
| ArrAt of expr * expr
| Call of string * expr list
| MethodCall of string * string * expr list
| EnumCase of string * string
| Self
| SelfField of string
| SelfCall of string * expr list
| SelfAssign of string * expr
| NoExpr

type stmt =
  Var of string * typ * expr
| Let of string * typ * expr
| Expr of expr
| Return of expr
| If of expr * block * block
| IfLet of string * typ * expr * block * block
| While of expr * block
| For of string * expr * block
and block = stmt list

type func_defn = {
  fname : string;
  args : (string * typ) list;
  ty : typ;
  body : stmt list;
}

type cls_defn = {
  cname : string;
  fields : (string * typ * expr) list;
  inits : ((string * typ) list * stmt list) list;
  methods : func_defn list;
}

type enum_defn = {
  ename : string;
  cases : string list;
}

type defn = Func_defn of func_defn | Cls_defn of cls_defn
          | Enum_defn of enum_defn | Stmt of stmt

type program = defn list

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Leq -> "<="
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"
  | Orange -> "..."
  | Crange -> "..<"

let string_of_uop = function
    Not -> "!"
  | Neg -> "-"

let rec string_of_literal = function
  | IntLit(i) -> string_of_int i
  | BoolLit(b) -> string_of_bool b
  | CharLit(c) -> "'" ^ Char.escaped c ^ "'"
  | FloatLit(f) -> string_of_float f
  | StringLit(s) -> "\"" ^ s ^ "\""
  | ArrayLit(ls) -> "[" ^ String.concat ", " (List.map string_of_literal ls) ^ "]"
  | Nil -> "nil"

let rec string_of_expr = function
    Literal(l) -> string_of_literal(l)
  | Id(s) -> s
  | Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ " " ^ string_of_expr e
  | Assign(s, e) -> s ^ " = " ^ string_of_expr e
  | ArrAssign(s, i, e2) -> string_of_expr s ^ "[" ^ string_of_expr i ^ "] = " ^ string_of_expr e2
  | ArrAt(s, i) -> string_of_expr s ^ "[" ^ string_of_expr i ^ "]"
  | Call(s, el) -> s ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | MethodCall(s1, s2, el) -> s1 ^ "." ^ string_of_expr (Call(s2, el))
  | EnumCase(s1, s2) -> s1 ^ "." ^ s2
  | Self -> "self"
  | SelfField(s) -> "self." ^ s
  | SelfCall(s, es) -> "self." ^ s ^ "(" ^ String.concat ", " (List.map string_of_expr es) ^ ")"
  | SelfAssign(s, e) -> "self." ^ s ^ " = " ^ string_of_expr e
  | NoExpr -> ""

let rec string_of_type = function
    Int -> "Int"
  | Float -> "Float"
  | Char -> "Char"
  | String -> "String"
  | Bool -> "Bool"
  | Optional(t) -> string_of_type t ^ "?"
  | UserDef(s) -> s
  | Array(t) -> "[" ^ string_of_type t ^ "]"

let rec string_of_stmt = function
    Var(s, t, e) -> "var " ^ s ^ ": " ^ string_of_type t ^ " = " ^ string_of_expr e ^ ";"
  | Let(s, t, e) -> "let " ^ s ^ ": " ^ string_of_type t ^ " = " ^ string_of_expr e ^ ";"
  | Expr(e) -> string_of_expr e ^ ";"
  | Return(e) -> "return " ^ string_of_expr e ^ ";"
  | If (e, ts, fs) -> let fString = match fs with [] -> "" | _ -> "else {\n" ^ String.concat "\n" (List.map string_of_stmt fs) ^ "\n}" in
      "if " ^ string_of_expr e ^ " {\n" ^
      String.concat "\n" (List.map string_of_stmt ts) ^ "\n}" ^ fString
  | IfLet(s, t, e, ts, fs) -> "if let " ^ s ^ ": " ^ string_of_type t ^ " = " ^ string_of_expr e ^ "{\n" ^
      String.concat "\n" (List.map string_of_stmt ts) ^ "\n} else {\n" ^ String.concat "\n" (List.map string_of_stmt fs) ^ "\n}"
  | While(e, ls) -> "while " ^ string_of_expr e ^ "{\n" ^ String.concat "\n" (List.map string_of_stmt ls) ^ "\n}"
  | For(s, e, ls) -> "for " ^ s ^ " in " ^ string_of_expr e ^ " {\n" ^ String.concat "\n" (List.map string_of_stmt ls) ^ "\n}"

let string_of_arg = function
    (s, t) -> s ^ ": " ^ string_of_type t

let string_of_func f =
  "func " ^ f.fname ^ "(" ^ String.concat ", " (List.map string_of_arg f.args) ^ ") -> " ^ string_of_type f.ty ^ "{\n"
  ^ String.concat "\n" (List.map string_of_stmt f.body) ^ "\n}"

let string_of_init = function
  (xs, ys) -> "init(" ^ String.concat ", " (List.map string_of_arg xs) ^ ") {\n" ^ String.concat "\n" (List.map string_of_stmt ys) ^ "\n}"

let string_of_cls c =
  "class " ^ c.cname ^ " {\n" ^ String.concat "\n" (List.map (fun (s, t, e) -> string_of_stmt (Var (s, t, e))) c.fields) ^
  String.concat "\n" (List.map string_of_init c.inits)^ "\n" ^ String.concat "\n" (List.map string_of_func c.methods) ^ "\n}"

let string_of_enum e =
  "enum " ^ e.ename ^ "{\n" ^ String.concat ";\n" (List.map (fun (s) -> "case " ^ s) e.cases) ^ "\n}"

let string_of_defn = function
    Func_defn(f) -> string_of_func f
  | Cls_defn(c) ->  string_of_cls c
  | Enum_defn(e) -> string_of_enum e
  | Stmt(s) -> string_of_stmt s

let string_of_program p = String.concat "\n" (List.map string_of_defn p)













