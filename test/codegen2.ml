open Brawn_parser
open Brawn_ast.Ast;;

open Lexing;;

exception Parse_error of exn * int * int * string;;

let handle_parse_error buf f =
  try
    f ()
  with
  | exn ->
    let curr = buf.lex_curr_p in
    let line = curr.pos_lnum in
    let column = curr.pos_cnum - curr.pos_bol in
    let tok = lexeme buf in
    raise @@ Parse_error(exn,line,column,tok)
;;

(* let parse_program (input : IO.input) = *)
let parse_program (input : string) : program =
  let buf = Lexing.from_string input in
  handle_parse_error buf @@ fun () ->
  Grammar.program Tokens.token buf
;;

let _ = print_endline @@ show_program @@ parse_program "BEGIN { ENVIRON[\"USER\"] = \"ayaka\"; print ENVIRON[\"USER\"]}; EOF"
(* "BEGIN { a=1 ; print a }; EOF" *)
;;
