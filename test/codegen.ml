open Llvm
open Brawn_ast.Ast


exception CodeGenError of string

(* Global contants *)
let context = create_context ()
let llvm_module = create_module context "brawn"
let builder = builder context
(* let named_values: (string, llvalue) Hashtbl.t = Hashtbl.create 10 *)

let brawn_value_type =
  named_struct_type context "struct.brawn.brawn_value"
let brawn_type = pointer_type brawn_value_type


let process_action_decl pat_list statement_list = ()


let codegen_function_proto name args = 
  let args_types = Array.make (Array.length args) brawn_type in
  let ft = function_type brawn_type args_types in
  match lookup_function name llvm_module with
  | None -> declare_function name ft llvm_module
  | Some _ -> raise (CodeGenError "redefinition of function")

let codegen_function_decl (Function (Identifier name, parameters, stmts) =
  (* probably don't want to clear out the named_values hashtable *)
  let the_function = codegen_function_proto name parameters in
  let bb = append_block context "entry" the_function in
  position_at_end bb builder;
  try
    let ret_val = codegen_expr body in 
    let _ = build_ret ret_val builder in
    Llvm_analysis.assert_valid_function the_function;
    the_function
  with e ->
    delete_function the_function;
    raise e



  
  
  let Function (Identifier(func_name) , arg_list, s_list) = func in
  let temp_mod = create_module ctx "brawn" in 
  let f = Llvm.define_function func_name (brawn_type ctx) temp_mod in 
  ctx
  (* we want to "register" this f into the context... *)

let codegen_item = function
  | FunctionDecl func -> codegen_function_decl func
  | ActionDecl (patterns, stmts) -> codegen_action_decl patterns stmts

(* TODO: this should be fleshed out further to take care of control flow etc *)
let codegen_program ((Program items) : program) ctx =
  List.map codegen_item items 
  
let id_function = 
  let decl = declare_function "id" (function_type brawn_type [|brawn_type|]) llvm_module in
  let entry = append_block ctx "entry" decl in
  position_at_end entry builder;
  let ret_val = param decl 0 in
  let _ = build_ret ret_val builder in
  Llvm_analysis.assert_valid_function decl;
  decl

let emit_llvm () =
  let _ = id_function in
  dump_module llvm_module

let _ = emit_llvm ()