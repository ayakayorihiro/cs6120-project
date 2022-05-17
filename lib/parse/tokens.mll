{ open Grammar;;
  open Lexing;;
  (* NOTE: modify the below based on whether you'd like to see what tokens are generatedb *)
  let debug_print content = 
  print_endline content
    (* () *)
  let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }
} 

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let rest = ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_']*
let whitespace = [' ' '\t']
let newline = '\n'
let string_contents = [^'"']*
let regex_contents = [^'/']*
let newline = '\n'
let built_in_names = "atan2"|"cos"|"sin"|"exp"|"log"|"sqrt"|"int"|"rand"|"srand"|"gsub"|"index"|"length"|"match"|"split"|"sprintf"|"sub"|"substr"|"tolower"|"toupper"|"close"|"system"

let ident_start = alpha
let ident_cont = alpha | digit | '_'

rule token = parse
  | eof    { debug_print "EOF"; EOF }
  | "EOF" { debug_print "alt-EOF" ; EOF}
  | [' ' '\t'] { debug_print "w" ; token lexbuf } (* skip whitespace *)
  | newline              { incr_lineno lexbuf; token lexbuf }
  | "BEGIN" { debug_print "BEGIN" ; Begin }
  | "END" { debug_print "END" ; End }
  | "break" { debug_print "Break"; Break }
  | "continue" { debug_print "Continue"; Continue }
  | "delete" { debug_print "Delete"; Delete }
  | "do" { debug_print "Do"; Do }
  | "else" { debug_print "Else"; Else }
  | "exit" { debug_print "Exit";Exit }
  | "for" { debug_print "For"; For }
  | "function" { debug_print "Function"; Function }
  | "if" { debug_print "If"; If }
  | "in" { debug_print "In"; In }
  | "next" { debug_print "Next" ; Next }
  | "print" { debug_print "Print" ; Print }
  | "printf" { debug_print "Printf"; Printf }
  | "return" { debug_print "Return"; Return }
  | "while" { debug_print "While"; While }
  | "+=" { debug_print "ADD_ASSIGN" ; ADD_ASSIGN }
  | "-=" { debug_print "SUB_ASSIGN" ; SUB_ASSIGN }
  | "*=" { debug_print "MUL_ASSIGN" ; MUL_ASSIGN }
  | "/=" { debug_print "DIV_ASSIGN" ; DIV_ASSIGN }
  | "%=" { debug_print "MOD_ASSIGN"; MOD_ASSIGN }
  | "^=" { debug_print "POW_ASSIGN"; POW_ASSIGN }
  | "||" { debug_print "OR" ; OR }
  | "&&" { debug_print "AND" ; AND }
  | "!~" { debug_print "NO_MATCH" ; NO_MATCH }
  | "==" { debug_print "EQ" ; EQ }
  | "<=" { debug_print "LE" ; LE }
  | ">=" { debug_print "GE" ; GE }
  | "!=" { debug_print "NE" ; NE }
  | "++" { debug_print "INCR";INCR }
  | "--" { debug_print "DECR";DECR }
  (* | ">>" { debug_print ;APPEND } *)
  | '{' { debug_print "LCURL";LCURL }
  | '}' { debug_print "RCURL";RCURL }
  | '(' { debug_print "LPAREN";LPAREN }
  | ')' { debug_print "RPAREN";RPAREN }
  | '[' { debug_print "LBRACK";LBRACK }
  | ']' { debug_print "RBRACK";RBRACK }
  | ',' { debug_print "COMMA";COMMA }
  | ';' { debug_print "SEMICOLON";SEMICOLON }
  | '+' { debug_print "PLUS";PLUS }
  | '-' { debug_print "MINUS";MINUS }
  | '*' { debug_print "MULT";MULT }
  | '%' { debug_print "MOD";MOD }
  | '^' { debug_print "POW";POW }
  | '/' { debug_print "DIV";DIV }
  | '!' { debug_print "BANG";BANG }
  | '>' { debug_print "GT";GT }
  | '<' { debug_print "LT";LT }
  | '|' { debug_print "PIPE";PIPE }
  | '?' { debug_print "QMARK";QMARK }
  | ':' { debug_print "COLON";COLON }
  | '~' { debug_print "SQUIGGLE";SQUIGGLE }
  | '$' { debug_print "DOLLAR";DOLLAR }
  | '=' { debug_print "ASSIGN";ASSIGN }
  | digit* '.' digit+ as n         { debug_print @@ "NUMBER " ^ n ; NUMBER (float_of_string n) }
  | digit+ as n         { debug_print @@ "NUMBER " ^ n ; NUMBER (float_of_string n) }
  (* Can we get rid of this, trusting Getline to return None when we hit eof? *)
  | "\"" (string_contents as s) "\"" { debug_print @@ "STRING " ^ s; STRING s }
  | "/" (regex_contents as r) "/" { debug_print @@ "REGEX " ^ r ; ERE r }
  | built_in_names as funcname { debug_print @@ "BUILTIN_FUNC_NAME " ^ funcname ; BUILTIN_FUNC_NAME funcname }
  (* | alpha rest '(' as funcname { debug_print @@ "FUNCNAME " ^ funcname; FUNC_NAME funcname } *)
  | ident_start ident_cont* as s { debug_print @@ "NAME " ^ s ; NAME s}
