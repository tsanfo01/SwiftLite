type token =
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | COMMA
  | SEMI
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | NOT
  | AND
  | OR
  | ASSIGN
  | ORANGE
  | CRANGE
  | LET
  | VAR
  | IN
  | IF
  | ELSE
  | FOR
  | WHILE
  | RETURN
  | INT
  | FLOAT
  | CHAR
  | STRING
  | BOOL
  | OPTIONAL
  | NIL
  | COLON
  | DOT
  | CLASS
  | INIT
  | SELF
  | FUNC
  | ARROW
  | ENUM
  | CASE
  | BOOLLIT of (bool)
  | INTLIT of (int)
  | CHARLIT of (char)
  | FLOATLIT of (float)
  | STRINGLIT of (string)
  | ID of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
