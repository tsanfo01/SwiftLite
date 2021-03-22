/* Ocamlyacc parser for SwiftLite */

%{
open Ast
%}

%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token COMMA SEMI PLUS MINUS TIMES DIVIDE MOD
%token EQ NEQ LT LEQ GT GEQ NOT AND OR ASSIGN ORANGE CRANGE
%token LET VAR IN IF ELSE FOR WHILE RETURN 
%token INT FLOAT CHAR STRING BOOL OPTIONAL NIL COLON DOT
%token CLASS INIT SELF FUNC ARROW ENUM CASE

%token <bool> BOOLLIT
%token <int> INTLIT
%token <char> CHARLIT
%token <float> FLOATLIT
%token <string> STRINGLIT ID
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE LBRACKET
%nonassoc ELSE LPAREN
%right ASSIGN
%right DOT
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ ORANGE CRANGE
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT

%%

program:
  defns EOF { List.rev $1 }

defns:
    /* nothing */ { [] }
  | defns fdefn { Func_defn($2)::$1 }
  | defns cdefn { Cls_defn($2)::$1  }
  | defns edefn { Enum_defn($2)::$1 }
  | defns stmt  { Stmt($2)::$1      }


fdefn:
  FUNC ID LPAREN args_opt RPAREN ARROW typ LBRACE stmt_list RBRACE {
    { fname = $2; args = List.rev $4; ty = $7; body = List.rev $9 }
  }

cdefn:
  CLASS ID LBRACE fields_opt init_list  methods_opt RBRACE {
    { cname = $2; fields = List.rev $4; inits = List.rev $5; methods = List.rev $6}
  }

edefn:
  ENUM ID LBRACE case_list RBRACE {
    { ename = $2; cases = List.rev $4 }
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
    INIT LPAREN args_opt RPAREN LBRACE stmt_list RBRACE           { [($3, $6)]     }
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
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                           {Expr $1}
  | VAR ID COLON typ ASSIGN expr SEMI   {Var($2,$4,$6)}
  | LET ID COLON typ ASSIGN expr SEMI   {Let($2,$4,$6)}
  | RETURN expr_opt SEMI                {Return $2}
  | cond                                { $1 }
  | WHILE expr LBRACE stmt_list RBRACE {While($2, List.rev $4)}
  | FOR ID IN expr LBRACE stmt_list RBRACE {For($2, $4, List.rev $6)}
cond:
    IF expr LBRACE stmt_list RBRACE %prec NOELSE {If($2, List.rev $4, [])}
  | IF expr LBRACE stmt_list RBRACE else_branch {If($2, List.rev $4, List.rev $6)}
  | IF LET ID COLON typ ASSIGN expr LBRACE stmt_list RBRACE else_branch {IfLet($3,$5,$7, List.rev $9, List.rev $11)}

else_branch:
    ELSE LBRACE stmt_list RBRACE { List.rev $3 }
  | ELSE cond { [$2] }

expr_opt:
    /* nothing */ { NoExpr }
  | expr          { $1 }

expr_list_opt:
    /* nothing */ { [] }
  | expr_list     { $1 }

expr_list:
    expr           { [$1]     }
  | expr_list COMMA expr { $3 :: $1 }

literal:
    BOOLLIT                   { BoolLit($1)          }
  | STRINGLIT                 { StringLit($1)        }
  | CHARLIT                   { CharLit($1)          }
  | INTLIT                    { IntLit($1)           }
  | FLOATLIT                  { FloatLit($1)         }
  | LBRACKET lit_opt RBRACKET { ArrayLit (List.rev $2) }
  | NIL                       { Nil                  }

lit_opt:
    /* nothing */ { [] }
  | lit_list      { $1 }

lit_list:
    literal                { [$1]     }
  | lit_list COMMA literal { $3 :: $1 }

expr:
    literal          { Literal $1             }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mul,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr MOD    expr { Binop($1, Mod,   $3)   }
  | expr EQ     expr { Binop($1, Eq,    $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr LT     expr { Binop($1, Lt,    $3)   }
  | expr LEQ    expr { Binop($1, Leq,   $3)   }
  | expr GT     expr { Binop($1, Gt,    $3)   }
  | expr GEQ    expr { Binop($1, Geq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | expr ORANGE expr { Binop($1, Orange, $3)  }
  | expr CRANGE expr { Binop($1, Crange, $3)  }
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  | LPAREN expr RPAREN { $2 }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | expr LBRACKET expr RBRACKET {ArrAt($1,$3)}
  | expr LBRACKET expr RBRACKET ASSIGN expr {ArrAssign($1,$3,$6)}
  | ID LPAREN expr_list_opt RPAREN {Call($1,$3)}
  | ID DOT ID LPAREN expr_list_opt RPAREN {MethodCall($1,$3,$5)}
  | ID DOT ID  {EnumCase($1,$3)}
  | SELF    { Self }
  | SELF DOT ID { SelfField($3) }
  | SELF DOT ID LPAREN expr_list_opt RPAREN { SelfCall($3, $5) }
  | SELF DOT ID ASSIGN expr { SelfAssign($3, $5) }

