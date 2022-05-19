{
    open Grammar
    open Lexing

    (* NOTE: modify the below based on whether you'd like to see what tokens are generatedb *)
    let debug_print _ = (* print_endline content *) ()

    exception SyntaxError of string

    let next_line lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
          { pos with pos_bol = lexbuf.lex_curr_pos;
                     pos_lnum = pos.pos_lnum + 1
          }
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let whitespace = [' ' '\t']
let newline = '\n'
let ident = alpha (alpha | digit | '_')*
let pm = ['+' '-']
let number = pm?digit+'.'?digit*(['e' 'E']pm?digit+)?

rule token = parse
    | eof    { debug_print "EOF"; EOF }
    | "EOF" { debug_print "alt-EOF" ; EOF}
    | [' ' '\t'] { debug_print "w" ; token lexbuf } (* skip whitespace *)
    | newline              { next_line lexbuf; token lexbuf }
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
    | '@' { debug_print "CONCAT"; CONCAT}
    | '?' { debug_print "QMARK";QMARK }
    | ':' { debug_print "COLON";COLON }
    | '~' { debug_print "SQUIGGLE";SQUIGGLE }
    | '$' { debug_print "DOLLAR";DOLLAR }
    | '=' { debug_print "ASSIGN";ASSIGN }
    | number as n { debug_print @@ "NUMBER " ^ n ; NUMBER (float_of_string n) }
    | '"'         { read_string (Buffer.create 256) lexbuf }
    | '/'         { read_regex (Buffer.create 256) lexbuf }
    | ident as s  { debug_print @@ "NAME " ^ s ; NAME s}

and read_string buf = parse
    | '"'       { STRING (Buffer.contents buf) }
    | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
    | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
    | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
    | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
    | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
    | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
    | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
    | [^ '"' '\\']+
      { Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_string buf lexbuf
      }
    | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
    | eof { raise (SyntaxError ("String is not terminated")) }

and read_regex buf = parse
    | '/'       { ERE (Buffer.contents buf) }
    | '\\' '/'  { Buffer.add_string buf "\\/"; read_regex buf lexbuf }
    | [^ '/']+
      { Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_regex buf lexbuf
      }
    | _ { raise (SyntaxError ("Illegal extended regex character: " ^ Lexing.lexeme lexbuf)) }
    | eof { raise (SyntaxError ("Extended regex is not terminated")) }
