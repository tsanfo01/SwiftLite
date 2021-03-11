type typ = Int | Float | Char | String | Bool | Optional of typ
         | Cls of string | Enum of string | Array of typ

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
| Call of string * expr list
| Init of string * (string * typ) list
| MethodCall of string * string * expr list
| EnumCase of string * string
| NoExpr


type stmt =
  Var of string * typ * expr
| Let of string * typ * expr
| Expr of expr
| Return of expr
| If of expr * block * block
| IfLet of string * typ * expr * block * block
| While of expr * block
| For of expr * expr * block
and block = stmt list

type func_defn = {
  name : string;
  args : (string * typ) list;
  ty : typ;
  body : stmt list;
}

type cls_defn = {
  name : string;
  init : (string * typ) list * stmt list;
  fields : (string * typ * expr) list;
  methods : func_defn list;
}

type enum_defn = {
  name : string;
  cases : string list;
}

type defn = Func_defn of func_defn | Cls_defn of cls_defn
          | Enum_defn of enum_defn | Stmt of stmt

type program = defn list