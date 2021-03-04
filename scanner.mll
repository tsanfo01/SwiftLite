{ open Parser }

let digit = ['0' - '9']
let digits = digit+
let character = ['a'-'z' 'A'-'Z' '0'-'9' '_']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| "//"     { comment lexbuf }
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
| "/"      { DIV }
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
| ":"      { COLON }
| "class"  { CLASS }
| "init"   { INIT }
| "self"   { SELF }
| "func"   { FUNC }
| "enum"   { ENUM }
| "case"   { CASE }
| "true"   { BOOLLIT(true) }
| "false"  { BOOLLIT(false) }
| digits as lxm { INTLIT(int_of_string lxm) }
| digits '.' digit* as lxm { FLOATLIT(lxm) }
| "'" character "'" as c { CHARLIT(c) }
| '"' character* '"' as s { STRINGLIT(s) }
| ['a'-'z' 'A'-'Z' ] character* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

rule comment = parse
  '\n' { token lexbuf }
| _    { comment lexbuf }

