open Brawn_parser
open Brawn_codegen
open Brawn_typecheck
open Lexing

exception BrawnError of string
exception ParseError of exn * int * int * string

let handle_parse_error buf f =
    try
        f ()
    with exn ->
        let curr = buf.lex_curr_p in
        let line = curr.pos_lnum in
        let column = curr.pos_cnum - curr.pos_bol in
        let tok = lexeme buf in
        raise (ParseError (exn, line, column, tok))

let parse_program input =
    let buf = Lexing.from_channel input in
    handle_parse_error buf (fun () -> Grammar.program Tokens.token buf)

let _ =
    if Array.length (Sys.argv) < 3 then raise (BrawnError "brawn: no file to compile.") else ();
    let input = Sys.argv.(1) in
    let output = Sys.argv.(2) in
    let program = parse_program (open_in input) in
    Typecheck.visit_program program;
    Llvm.set_target_triple (Llvm.target_triple Codegen.runtime_module) Codegen.program_module;
    Llvm.set_data_layout (Llvm.data_layout Codegen.runtime_module) Codegen.program_module;
    Codegen.codegen_runtime_funcs ();
    Hashtbl.iter Codegen.codegen_global Typecheck.globals;
    Hashtbl.iter Codegen.codegen_literal Typecheck.constants;
    Codegen.codegen_program program;
    Llvm.print_module output Codegen.program_module;
