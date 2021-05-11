{
open Parser
}
let digit = ['0' - '9']
let digits = digit+
let character = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let strCharacter = [ ' ' '!' '#' - '~' ]


rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| "//"     { inlineComment lexbuf }
| "/*"     { blockComment lexbuf }
| "("      { LPAREN }
| ")"      { RPAREN }
| "{"      { LBRACE }
| "}"      { RBRACE }
| "["      { LBRACKET }
| "]"      { RBRACKET }
| ","      { COMMA }
| ";"      { SEMI }
| "+"      { PLUS }
| "-"      { MINUS }
| "*"      { TIMES }
| "/"      { DIVIDE }
| "%"      { MOD }
| "=="     { EQ }
| "!="     { NEQ }
| "<"      { LT }
| ">"      { GT }
| "<="     { LEQ }
| ">="     { GEQ }
| "!"      { NOT }
| "&&"     { AND }
| "||"     { OR }
| "="      { ASSIGN }
| "..."    { ORANGE }
| "..<"    { CRANGE }
| "let"    { LET }
| "var"    { VAR }
| "in"     { IN }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "Int"    { INT }
| "Float"  { FLOAT }
| "Char"   { CHAR }
| "String" { STRING }
| "Bool"   { BOOL }
| "?"      { OPTIONAL }
| "nil"    { NIL }
| ":"      { COLON }
| "."      { DOT }
| "self"   { SELF }
| "class"  { CLASS }
| "init"   { INIT }
| "func"   { FUNC }
| '-''>'   { ARROW }
| "enum"   { ENUM }
| "case"   { CASE }
| "true"   { BOOLLIT(true) }
| "false"  { BOOLLIT(false) }
| digits as lxm { INTLIT(int_of_string lxm) }
| ('-' digits) as lxm { INTLIT(int_of_string lxm) }
| digits '.' digits as lxm { FLOATLIT(float_of_string lxm) }
| ('-' digits '.' digits) as lxm { FLOATLIT(float_of_string lxm) }
| "'" (_ as c) "'" { CHARLIT(c) }
| '"' (strCharacter* as s) '"' { STRINGLIT(s) }
| ['a'-'z' 'A'-'Z' ] character* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and inlineComment = parse
  '\n' { token lexbuf }
| eof  { EOF }
| _    { inlineComment lexbuf }

and blockComment = parse
  "*/" { token lexbuf }
| _    { blockComment lexbuf }
