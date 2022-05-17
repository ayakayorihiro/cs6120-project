open Llvm
open Brawn_ast.Ast

exception CodeGenError of string

(* Global constants *)
let context = create_context ()
let llvm_module = create_module context "brawn"
let builder = builder context
let named_values: (string, llvalue) Hashtbl.t = Hashtbl.create 10

let brawn_value_type = named_struct_type context "struct.brawn.brawn_value"
let brawn_type = pointer_type brawn_value_type


(* todo remove *)
let unimplemented = const_string context "unimplemented"

(* in brawn this is just a helper *)
let codegen_function_proto name args = 
  let args_types = Array.make (List.length args) brawn_type in
  let ft = function_type brawn_type args_types in
  match lookup_function name llvm_module with
  | None -> declare_function name ft llvm_module
  | Some _ -> raise (CodeGenError "redefinition of function")

let codegen_expr expr = 
  match expr with
  | Input i -> unimplemented
  | FuncCall (name, args) -> unimplemented
  | Plus (e1, e2) 
  | Subtract (e1, e2) 
  | Multiply (e1, e2)
  | Divide (e1, e2) 
  | Mod (e1, e2)
  | Pow (e1, e2)
  | LessThan (e1, e2)
  | LessThanEq (e1, e2)
  | Equals (e1, e2)
  | NotEquals (e1, e2)
  | GreaterThan (e1, e2)
  | GreaterThanEq (e1, e2)
  | Match (e1, e2)
  | NonMatch (e1, e2)
  | And (e1, e2)
  | Or (e1, e2)
  | Concat (e1, e2) -> unimplemented
  | Regexp str -> unimplemented
  | Ternary (e1, e2, e3) -> unimplemented
  | Mem (e, array_name) -> unimplemented
  | Negative e -> unimplemented
  | Positive e -> unimplemented
  | Not e -> unimplemented
  | LIncr lvalue 
  | LDecr lvalue
  | RIncr lvalue
  | RDecr lvalue
  | LValue lvalue -> unimplemented
  | Number f -> unimplemented
  | String s -> unimplemented
  | PowAssign (lvalue, e)
  | ModAssign (lvalue, e)
  | MulAssign (lvalue, e)
  | DivAssign (lvalue, e)
  | AddAssign (lvalue, e)
  | SubAssign (lvalue, e)
  | Assignment (lvalue, e) -> unimplemented


let codegen_function_decl (Function (Identifier name, parameters, body)) =
  (* probably don't want to clear out the named_values hashtable, 
     as the guide says to. *)
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

let codegen_action_decl pattern body = unimplemented
  (* if we match the pattern, run the body *)

let codegen_item = function
  | FunctionDecl func -> codegen_function_decl func
  | ActionDecl (pattern, stmts) -> codegen_action_decl pattern stmts

(* TODO: this should be fleshed out further to take care of control flow etc *)
let codegen_program ((Program items) : program) =
  List.map codegen_item items 
  
let id_function = 
  let decl = declare_function "id" (function_type brawn_type [|brawn_type|]) llvm_module in
  let entry = append_block context "entry" decl in
  position_at_end entry builder;
  let ret_val = param decl 0 in
  let _ = build_ret ret_val builder in
  Llvm_analysis.assert_valid_function decl;
  decl

let emit_llvm () =
  let _ = id_function in
  dump_module llvm_module

let _ = emit_llvm ()