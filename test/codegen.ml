open Llvm
(* open Brawn_ast.Ast *)

(* Global contants *)
let ctx = create_context ()
let llvm_module = create_module ctx "brawn"
let builder = builder ctx
(* let named_values: (string, llvalue) Hashtbl.t = Hashtbl.create 10 *)
let brawn_value_type =
  named_struct_type ctx "struct.brawn.brawn_value"
let brawn_type = pointer_type brawn_value_type

(* let process_action_decl ctx pat_list statement_list : llcontext =
  ctx

let process_function_decl ctx func : llcontext =
  let Function (Identifier(func_name) , arg_list, s_list) = func in
  let temp_mod = create_module ctx "brawn" in 
  let f = Llvm.define_function func_name (brawn_type ctx) temp_mod in 
  ctx
  (* we want to "register" this f into the context... *)


let process_one_item ctx item : llcontext = 
  match item with
  | FunctionDecl (func) ->
    process_function_decl ctx func
  | ActionDecl (pat_list, statement_list) ->  
    process_action_decl ctx pat_list statement_list

let process_item_list ((Program items) : program) ctx =
  List.fold_left process_one_item ctx items 
   *)

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