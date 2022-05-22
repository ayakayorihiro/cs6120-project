open Brawn_parser
open Brawn_typecheck
open Brawn_codegen
open Lexing

let handle_parse_error buf f =
    try
        f ()
    with _ ->
        let curr = buf.lex_curr_p in
        let line = curr.pos_lnum in
        let column = curr.pos_cnum - curr.pos_bol in
        let tok = lexeme buf in
        print_endline ((string_of_int line) ^ ":" ^ (string_of_int column) ^ " error occured parsing " ^ tok ^ ".");
        exit 1

let parse_program input =
    let buf = Lexing.from_channel input in
    handle_parse_error buf (fun () -> Grammar.program Tokens.token buf)

let _ =
    if Array.length (Sys.argv) < 3 then
        (print_endline "usage: brawnc runtime_module.ll input_file.brawn output_file.ll"; exit 1);
    let input = Sys.argv.(2) in
    let output = Sys.argv.(3) in
    let program = parse_program (open_in input) in
    Typecheck.visit_program program;
    Codegen.codegen_program program;
    Llvm.print_module output Codegen.program_module
