{ open Grammar } 

let alpha = ['a'-'z' 'A'-'Z']
let rest = ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_']*
let whitespace = [' ' '\t']
let newline = '\n'
rule token = parse
  | [' ' '\t'] { token lexbuf } (* skip whitespace *)
  | "BEGIN" { Begin }
  | "END" { End }
  | "break" { Break }
  | "continue" { Continue }
  | "delete" { Delete }
  | "do" { Do }
  | "else" { Else }
  | "exit" { Exit }
  | "for" { For }
  | "function" { Function }
  | "if" { If }
  | "in" { In }
  | "next" { Next }
  | "print" { Print }
  | "printf" { Printf }
  | "return" { Return }
  | "while" { While }
  | "+=" { ADD_ASSIGN }
  | "-=" { SUB_ASSIGN }
  | "*=" { MUL_ASSIGN }
  | "/=" { DIV_ASSIGN }
  | "%=" { MOD_ASSIGN }
  | "^=" { POW_ASSIGN }
  | "||" { OR }
  | "&&" { AND }
  | "!~" { NO_MATCH }
  | "==" { EQ }
  | "<=" { LE }
  | ">=" { GE }
  | "!=" { NE }
  | "++" { INCR }
  | "--" { DECR }
  | ">>" { APPEND }
  | '{' { LCURL }
  | '}' { RCURL }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACK }
  | ']' { RBRACK }
  | ',' { COMMA }
  | ';' { SEMICOLON }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULT }
  | '%' { MOD }
  | '^' { POW }
  | '!' { BANG }
  | '>' { GT }
  | '<' { LT }
  | '|' { PIPE }
  | '?' { QMARK }
  | ':' { COLON }
  | '~' { SQUIGGLE }
  | '$' { DOLLAR }
  | eof    { EOL }
  | alpha rest as id { VAR id }
