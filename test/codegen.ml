open Llvm
open Brawn_ast.Ast

exception CodeGenError of string

(* Global constants *)
let context = create_context ()
let llvm_module = create_module context "brawn"
let builder = builder context
let named_values: (string, llvalue) Hashtbl.t = Hashtbl.create 10
let _ = named_values

let brawn_value_type = named_struct_type context "struct.brawn.brawn_value"
let brawn_type = pointer_type brawn_value_type


(* todo eventually remove *)
let unimplemented = const_string context "unimplemented"

(* in brawn this is just a helper *)
let codegen_function_proto name args = 
  let args_types = Array.make (List.length args) brawn_type in
  let ft = function_type brawn_type args_types in
  match lookup_function name llvm_module with
  | None -> declare_function name ft llvm_module
  | Some _ -> raise (CodeGenError "redefinition of function")

  
(* The operands e1 and e2 have already been reduced. 
   Now do the final binary tie-up.
   Note that we don't need to look at the operands all over again; 
   we are just here for the code this time 
*)
let codegen_binary_expr expr e1 e2 =
  match expr with 
  | Plus _ -> build_fadd e1 e2 "addtmp" builder
  | Subtract _ -> build_fsub e1 e2 "subtmp" builder
  | Multiply _ -> build_fmul e1 e2 "multmp" builder
  | Divide _ -> build_fdiv e1 e2 "divtmp" builder
  | Mod _
  | Pow _ -> unimplemented
  | LessThan _ -> build_fcmp Fcmp.Ult e1 e2 "lttmp" builder
  | LessThanEq _ 
  | Equals _ 
  | NotEquals _ 
  | GreaterThan _ 
  | GreaterThanEq _ 
  | Match _ 
  | NonMatch _ -> unimplemented
  | And _ -> build_and e1 e2 "andtmp" builder
  | Or _ -> build_or e1 e2 "ortmp" builder
  | Concat _ -> unimplemented
  | _ -> raise (CodeGenError "Can't get here")

let rec codegen_expr expr = 
  match expr with
  | Input _ -> unimplemented (* todo get a helper involved *)
  | FuncCall _ -> unimplemented (* see Kaleidoscope guide *)
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
  | Concat (e1, e2) -> 
      (* it makes sense to handle all binary cases in one way. 
         we then use a helper to go _back_ into the expr to see what code
         is needed to tie e1' and e2' together *)
      let e1' = codegen_expr e1 in 
      let e2' = codegen_expr e2 in 
      codegen_binary_expr expr e1' e2'
  | Regexp _ -> unimplemented
  | Ternary _ -> unimplemented (* const_select? *)
  | Mem _ -> unimplemented
  | Negative _
  | Positive _
  | Not _ -> unimplemented
  | LIncr _ 
  | LDecr _
  | RIncr _
  | RDecr _
  | LValue _ -> 
      (* similarly, it makes sense to handle all these cases in one way,
         then take cases on expr to see what specific thing
         needs to be done for them *)
          unimplemented
  | Number f -> const_float brawn_type f
  | String s -> const_stringz context s
  | PowAssign (_, e)
  | ModAssign (_, e)
  | MulAssign (_, e)
  | DivAssign (_, e)
  | AddAssign (_, e)
  | SubAssign (_, e)
  | Assignment (_, e) -> 
      (* again, do it once and then disambiguate again *)
      let e' = codegen_expr e in
      let _ = e' in 
      (* ... *)
      unimplemented

let codegen_stmt _ = unimplemented
    
let codegen_action_decl _ _ = unimplemented
  (* if we match the pattern, run the body *)

let codegen_function_decl (Function (Identifier name, parameters, body)) =
  (* probably don't want to clear out the named_values hashtable, 
      as the guide says to. *)
  let the_function = codegen_function_proto name parameters in
  let bb = append_block context "entry" the_function in
  position_at_end bb builder;
  try
    let ret_val = codegen_stmt body in 
    let _ = build_ret ret_val builder in
    Llvm_analysis.assert_valid_function the_function;
    the_function
  with e ->
    delete_function the_function;
    raise e


let codegen_item = function
  | FunctionDecl func -> codegen_function_decl func
  | ActionDecl (pattern, stmt) -> codegen_action_decl pattern stmt

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
let _ = codegen_expr in
let _ = codegen_program in
  let _ = id_function in
  dump_module llvm_module

let _ = emit_llvm ()