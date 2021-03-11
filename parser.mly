/* Ocamlyacc parser for SwiftLite */

%{
open Ast
%}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token COMMA SEMI PLUS MINUS TIMES DIV MOD
%token EQ NEQ LT LEQ GT GEQ NOT AND OR ASSIGN ORANGE CRANGE
%token LET VAR IN IF ELSE FOR WHILE RETURN
%token INT FLOAT CHAR STRING BOOL OPTIONAL NIL COLON
%token CLASS INIT SELF FUNC ARROW ENUM CASE
%token <bool> BOOLLIT
%token <int> INTLIT
%token <char> CHARLIT
%token <string> FLOATLIT STRINGLIT ID
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ ORANGE CRANGE
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT

%%

program:
  defns EOF { $1 }

defns:
    /* nothing */ { [] }
  | defns fdefn { Func_defn($2)::$1 }
  | defns cdefn { Cls_defn($2)::$1  }
  | defns edefn { Enum_defn($2)::$1 }
  | defns stmt  { Stmt($2)::$1      }


