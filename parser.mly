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

fdefn:
  FUNC ID LPAREN args_opt RPAREN ARROW typ LBRACE stmt_list RBRACE {
    { name = $2; args = List.rev $4; typ = $7; body = List.rev $9 }
  }

cdefn:
  CLASS ID LBRACE fields_opt init_list  methods_opt RBRACE {
    { name = $2; fields = List.rev $4; inits = List.rev $5; methods = List.rev $6}
  }

edefn:
  ENUM ID LBRACE case_list RBRACE {
    { name = $2; cases = List.rev $4 }
  }

args_opt:
    /* nothing */ { [] }
  | args_list     { $1 }

args_list:
    ID COLON typ                 { [($1, $3)]   }
  | args_list COMMA ID COLON typ { ($3, $5)::$1 }

fields_opt:
    /* nothing */ { [] }
  | fields_list   { $1 }

fields_list:
    VAR ID COLON typ assign_opt SEMI             { [($2, $4, $5)]   }
  | fields_list VAR ID COLON typ assign_opt SEMI { ($3, $5, $6)::$1 }

assign_opt:
    /* nothing */ { NoExpr }
  | ASSIGN expr   { $2     }

init_list:
    INIT LPAREN args_opt RPAREN LBRACE stmt_list RBRACE           { ($3, $6)     }
  | init_list INIT LPAREN args_opt RPAREN LBRACE stmt_list RBRACE { ($4, $7)::$1 }

methods_opt:
    /* nothing */ { [] }
  | methods_list  { $1 }

methods_list:
    fdefn              { [$1]   }
  | methods_list fdefn { $2::$1 }

case_list:
    CASE ID SEMI           { [$2]   }
  | case_list CASE ID SEMI { $3::$1 }

typ:
    INT                   { Int          }
  | BOOL                  { Bool         }
  | FLOAT                 { Float        }
  | CHAR                  { Char         }
  | STRING                { String       }
  | typ OPTIONAL          { Optional($1) }
  | ID                    { UserDef($1)  }
  | LBRACKET typ RBRACKET { Array($2)    }

stmt_list:
  /* nothing */ { [] }

expr:
  /* nothing */ { NoExpr }




