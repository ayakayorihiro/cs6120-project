{
    open Grammar
    open Lexing

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
let comment = '#' [^ '\n']* '\n'
let whitespace = [' ' '\t']
let newline = '\n'
let ident = ( alpha | '_' ) (alpha | digit | '_')*
let pm = ['+' '-']
let number = digit+'.'?digit*(['e' 'E']pm?digit+)?

rule token = parse
    | eof         { EOF }
    | [' ' '\t']  { token lexbuf } (* skip whitespace *)
    | newline     { next_line lexbuf; token lexbuf }
    | "BEGIN"     { Begin }
    | "END"       { End }
    | "break"     { Break }
    | "continue"  { Continue }
    | "delete"    { Delete }
    | "do"        { Do }
    | "else"      { Else }
    | "exit"      { Exit }
    | "for"       { For }
    | "function"  { Function }
    | "if"        { If }
    | "in"        { In }
    | "next"      { Next }
    | "print"     { Print }
    | "return"    { Return }
    | "while"     { While }
    | "getline"   { GETLINE }
    | number as n { NUMBER (float_of_string n) }
    | '"'         { read_string (Buffer.create 256) lexbuf }
    | "/#"        { read_regex (Buffer.create 256) lexbuf }
    | ident as s  { NAME s}
    | "+="        { ADD_ASSIGN }
    | "-="        { SUB_ASSIGN }
    | "*="        { MUL_ASSIGN }
    | "/="        { DIV_ASSIGN }
    | "%="        { MOD_ASSIGN }
    | "^="        { POW_ASSIGN }
    | "||"        { OR }
    | "&&"        { AND }
    | "!~"        { NO_MATCH }
    | "=="        { EQ }
    | "<="        { LE }
    | ">="        { GE }
    | "!="        { NE }
    | "++"        { INCR }
    | "--"        { DECR }
    | '{'         { LCURL }
    | '}'         { RCURL }
    | '('         { LPAREN }
    | ')'         { RPAREN }
    | '['         { LBRACK }
    | ']'         { RBRACK }
    | ','         { COMMA }
    | ';'         { SEMICOLON }
    | '+'         { PLUS }
    | '-'         { MINUS }
    | '*'         { MULT }
    | '%'         { MOD }
    | '^'         { POW }
    | '/'         { DIV }
    | '!'         { BANG }
    | '>'         { GT }
    | '<'         { LT }
    | '@'         { CONCAT}
    | '?'         { QMARK }
    | ':'         { COLON }
    | '~'         { SQUIGGLE }
    | '$'         { DOLLAR }
    | '='         { ASSIGN }
    | comment     { next_line lexbuf; token lexbuf }

and read_string buf = parse
    | '"'       { STRING (Buffer.contents buf) }
    | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
    | '\\' '"'  { Buffer.add_char buf '"'; read_string buf lexbuf }
    | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
    | '\\' 'a'  { Buffer.add_char buf '\007'; read_string buf lexbuf }
    | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
    | '\\' 'v'  { Buffer.add_char buf '\011'; read_string buf lexbuf }
    | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
    | '\\' '?'  { Buffer.add_char buf '\063'; read_string buf lexbuf }
    | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
    | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
    | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
    | [^ '"' '\\']+
      { Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_string buf lexbuf
      }
    | _ { raise (SyntaxError ("brawn: illegal string character: " ^ Lexing.lexeme lexbuf ^ ".")) }
    | eof { raise (SyntaxError ("brawn: string is not terminated.")) }

and read_regex buf = parse
    | "#/"      { ERE (Buffer.contents buf) }
    | '\\' '#'  { Buffer.add_char buf '#'; read_regex buf lexbuf }
    | '\\' '/'  { Buffer.add_char buf '/'; read_regex buf lexbuf }
    | '\\' 'a'  { Buffer.add_char buf '\007'; read_regex buf lexbuf }
    | '\\' 'b'  { Buffer.add_char buf '\b'; read_regex buf lexbuf }
    | '\\' 'v'  { Buffer.add_char buf '\011'; read_regex buf lexbuf }
    | '\\' 'f'  { Buffer.add_char buf '\012'; read_regex buf lexbuf }
    | '\\' '?'  { Buffer.add_char buf '\063'; read_regex buf lexbuf }
    | '\\' 'n'  { Buffer.add_char buf '\n'; read_regex buf lexbuf }
    | '\\' 'r'  { Buffer.add_char buf '\r'; read_regex buf lexbuf }
    | '\\' 't'  { Buffer.add_char buf '\t'; read_regex buf lexbuf }
    | [^ '#' ]+
      { Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_regex buf lexbuf
      }
    | _ { raise (SyntaxError ("brawn: illegal extended regex character: " ^ Lexing.lexeme lexbuf ^ ".")) }
    | eof { raise (SyntaxError ("brawn: extended regex is not terminated.")) }
