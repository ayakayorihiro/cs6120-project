open Llvm
open Brawn_ast.Ast

let brawn_type ctx =
  (* need to actually flesh this out.
     struggling to come up with str lltype 
  *)
  let num = i32_type ctx in 
  struct_type ctx [| num; num; num; num |]

let process_action_decl ctx pat_list statement_list : llcontext =
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
  
let emit_llvm _ = 
  let ctx = create_context () in 
  print_string (string_of_lltype (integer_type ctx 32))
  (* process_item_list program ctx *)

let _ = emit_llvm ()