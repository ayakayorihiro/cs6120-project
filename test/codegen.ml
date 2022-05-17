open Llvm
open Brawn_ast.Ast

exception CodeGenError of string

(* Global constants *)
let context = create_context ()
let runtime_module = Llvm_irreader.parse_ir context (Llvm.MemoryBuffer.of_file "../runtime/brawn_runtime.ll")
let program_module = create_module context "brawn"
let builder = builder context
let named_values: (string, llvalue) Hashtbl.t = Hashtbl.create 10
let _ = named_values

let brawn_value_type = Option.get (type_by_name runtime_module "struct::brawn.brawn_value")
let brawn_type = pointer_type brawn_value_type

(* Declare all runtime functions *)
let brawn_init = Option.get (lookup_function "brawn_init" runtime_module)
let brawn_from_number = Option.get (lookup_function "brawn_from_number" runtime_module)
let brawn_from_string = Option.get (lookup_function "brawn_from_string" runtime_module)
let brawn_from_regex = Option.get (lookup_function "brawn_from_regex" runtime_module)
let brawn_init_array = Option.get (lookup_function "brawn_init_array" runtime_module)
let brawn_assign = Option.get (lookup_function "brawn_assign" runtime_module)
let brawn_index = Option.get (lookup_function "brawn_index" runtime_module)
let brawn_not = Option.get (lookup_function "brawn_not" runtime_module)
let brawn_neg = Option.get (lookup_function "brawn_neg" runtime_module)
let brawn_add = Option.get (lookup_function "brawn_add" runtime_module)
let brawn_sub = Option.get (lookup_function "brawn_sub" runtime_module)
let brawn_mult = Option.get (lookup_function "brawn_mult" runtime_module)
let brawn_div = Option.get (lookup_function "brawn_div" runtime_module)
let brawn_pow = Option.get (lookup_function "brawn_pow" runtime_module)
let brawn_mod = Option.get (lookup_function "brawn_mod" runtime_module)
let brawn_and = Option.get (lookup_function "brawn_and" runtime_module)
let brawn_or = Option.get (lookup_function "brawn_or" runtime_module)
let brawn_lt = Option.get (lookup_function "brawn_lt" runtime_module)
let brawn_gt = Option.get (lookup_function "brawn_gt" runtime_module)
let brawn_le = Option.get (lookup_function "brawn_le" runtime_module)
let brawn_ge = Option.get (lookup_function "brawn_ge" runtime_module)
let brawn_eq = Option.get (lookup_function "brawn_eq" runtime_module)
let brawn_ne = Option.get (lookup_function "brawn_ne" runtime_module)
let brawn_concat = Option.get (lookup_function "brawn_concat" runtime_module)
let brawn_match = Option.get (lookup_function "brawn_match" runtime_module)
let brawn_match_regex = Option.get (lookup_function "brawn_match_regex" runtime_module)
let brawn_atan2 = Option.get (lookup_function "brawn_atan2" runtime_module)
let brawn_cos = Option.get (lookup_function "brawn_cos" runtime_module)
let brawn_sin = Option.get (lookup_function "brawn_sin" runtime_module)
let brawn_exp = Option.get (lookup_function "brawn_exp" runtime_module)
let brawn_log = Option.get (lookup_function "brawn_log" runtime_module)
let brawn_sqrt = Option.get (lookup_function "brawn_sqrt" runtime_module)
let brawn_int = Option.get (lookup_function "brawn_int" runtime_module)
let brawn_rand = Option.get (lookup_function "brawn_rand" runtime_module)
let brawn_srand_time = Option.get (lookup_function "brawn_srand_time" runtime_module)
let brawn_srand = Option.get (lookup_function "brawn_srand" runtime_module)
let brawn_string_index = Option.get (lookup_function "brawn_string_index" runtime_module)
let brawn_length = Option.get (lookup_function "brawn_length" runtime_module)
let brawn_substr = Option.get (lookup_function "brawn_substr" runtime_module)
let brawn_tolower = Option.get (lookup_function "brawn_tolower" runtime_module)
let brawn_toupper = Option.get (lookup_function "brawn_toupper" runtime_module)
let brawn_system = Option.get (lookup_function "brawn_system" runtime_module)


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
let codegen_binary_expr expr args =
  match expr with 
  | Plus _ -> build_call brawn_add args "addtmp" builder
  | Subtract _ -> build_call brawn_sub args "subtmp" builder
  | Multiply _ -> build_call brawn_mult args "multmp" builder
  | Divide _ -> build_call brawn_div args "divtmp" builder
  | Mod _ -> build_call brawn_mod args "modtmp" builder
  | Pow _ -> build_call brawn_pow args "powtmp" builder
  | LessThan _ -> build_call brawn_lt args "lttmp" builder
  | LessThanEq _ -> build_call brawn_le args "letmp" builder
  | Equals _ -> build_call brawn_eq args "eqtmp" builder
  | NotEquals _ -> build_call brawn_ne args "netmp" builder
  | GreaterThan _ -> build_call brawn_gt args "gttmp" builder
  | GreaterThanEq _ -> build_call brawn_ge args "getmp" builder
  | And _ -> build_call brawn_and args "andtmp" builder
  | Or _ -> build_call brawn_or args "ortmp" builder
  | Concat _ -> build_call brawn_concat args "concattmp" builder
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
  | And (e1, e2)
  | Or (e1, e2)
  | Concat (e1, e2) -> 
      (* it makes sense to handle all binary cases in one way. 
         we then use a helper to go _back_ into the expr to see what code
         is needed to tie e1' and e2' together *)
      let e1' = codegen_expr e1 in 
      let e2' = codegen_expr e2 in 
      codegen_binary_expr expr [|e1'; e2'|]
  | Match _ -> unimplemented
  | NonMatch _ -> unimplemented
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