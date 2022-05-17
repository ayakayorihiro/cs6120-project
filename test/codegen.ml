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

let build_call_from_runtime name args builder = 
  let func = Option.get (lookup_function "brawn_init" runtime_module) in 
  build_call func args (name^"tmp") builder

(* todo eventually remove *)
let unimplemented = const_string context "unimplemented"

(* in brawn this is just a helper *)
let codegen_function_proto name args = 
  let args_types = Array.make (List.length args) brawn_type in
  let ft = function_type brawn_type args_types in
  match lookup_function name program_module with
  | None -> declare_function name ft program_module
  | Some _ -> raise (CodeGenError "redefinition of function")

  
(* The operands e1 and e2 have already been reduced. 
   Now do the final binary tie-up.
   Note that we don't need to look at the operands all over again; 
   we are just here for the code this time 
*)
let codegen_binary_expr expr args =
  let helper name = build_call_from_runtime name args builder in 
  match expr with 
  | Plus _ -> helper "brawn_add"
  | Subtract _ -> helper "brawn_sub"
  | Multiply _ -> helper "brawn_mult"
  | Divide _ -> helper "brawn_div"
  | Mod _ -> helper "brawn_mod"
  | Pow _ -> helper "brawn_pow"
  | LessThan _ -> helper "brawn_lt"
  | LessThanEq _ -> helper "brawn_le"
  | Equals _ -> helper "brawn_eq"
  | NotEquals _ -> helper "brawn_ne"
  | GreaterThan _ -> helper "brawn_gt"
  | GreaterThanEq _ -> helper "brawn_ge"
  | And _ -> helper "brawn_and"
  | Or _ -> helper "brawn_or"
  | Concat _ -> helper "brawn_concat"
  | _ -> raise (CodeGenError "Can't get here")

let rec codegen_expr expr = 
  match expr with
  | Input _ -> unimplemented
  | FuncCall _ -> unimplemented
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
  | Match (e1, e2) -> 
      (* this has different treatment depending on whether e2 is a Regexp *)
      let e1' = codegen_expr e1 in 
      ( match e2 with 
        | Regexp str -> ignore str; unimplemented
          (* TODO this is wrong; I'm wondering how to pass the 
             std::regex that is expected by your signature when the build_call
             expects an llvalue array *)
        | _ -> build_call_from_runtime "brawn_match" [|e1'; (codegen_expr e2)|] builder
      )
  | NonMatch _ -> unimplemented
  | Regexp _ -> unimplemented
  | Ternary (e1, e2, e3) -> 
      let e1' = codegen_expr e1 in
      let e2' = codegen_expr e2 in
      let e3' = codegen_expr e3 in
      let cond = build_call_from_runtime "brawn_is_true" [|e1'|] builder in
      build_select cond e2' e3' "ternary_temp" builder
  | Mem _ -> unimplemented
  | Negative e -> 
      let e' = codegen_expr e in 
      build_call_from_runtime "brawn_neg" [|e'|] builder
  | Positive e -> (* just id? *)
      codegen_expr e 
  | Not e -> let e' = codegen_expr e in build_call_from_runtime "brawn_not" [|e'|] builder
  | LIncr lv (* treating a++ and ++a the same for now *)
  | RIncr lv -> codegen_expr (Assignment (lv, Plus (LValue lv, Number 1.0))) 
  | LDecr lv (* treating a-- and --a the same for now *)
  | RDecr lv -> codegen_expr (Assignment (lv, Subtract (LValue lv, Number 1.0)))
  | LValue _ -> unimplemented
  | Number f -> const_float brawn_type f
  | String s -> const_stringz context s
  | PowAssign (lv, e) -> codegen_expr (Assignment (lv, Pow (LValue lv, e)))
  | ModAssign (lv, e) -> codegen_expr (Assignment (lv, Mod (LValue lv, e)))
  | MulAssign (lv, e) -> codegen_expr (Assignment (lv, Multiply (LValue lv, e)))
  | DivAssign (lv, e) -> codegen_expr (Assignment (lv, Divide (LValue lv, e)))
  | AddAssign (lv, e) -> codegen_expr (Assignment (lv, Plus (LValue lv, e)))
  | SubAssign (lv, e) -> codegen_expr (Assignment (lv, Subtract (LValue lv, e)))
  | Assignment (lv, e) -> 
      let lv' = codegen_expr (LValue lv) in
      let e' = codegen_expr e in 
      build_call_from_runtime "brawn_assign" [|lv'; e'|] builder


let codegen_ifthenelse guard then_stmt else_stmt = unimplemented

let rec codegen_stmt stmt = 
  match stmt with    
  | If (guard, then_stmt, Some else_stmt) -> unimplemented
  | If (guard, then_stmt, None) -> codegen_stmt (If (guard, then_stmt, Some Skip))
  | While (guard, body) -> unimplemented
  | Do (body, guard) -> unimplemented
  | For (init_opt, guard_opt, update_opt, body) -> unimplemented
  | RangedFor (ele, array, body) -> unimplemented
  | Break
  | Continue
  | Next -> unimplemented
  | Exit (Some e) -> unimplemented
  | Exit None -> unimplemented
  | Return (Some e) -> let e' = codegen_expr e in build_ret e' builder
  | Return None -> build_ret_void builder
  | Delete (name, exprs) -> unimplemented
    (* not sure what's going on here.
       the guide gives the use as  
          delete array[index]
     *)
  | Print exprs -> unimplemented
  | Expression e -> codegen_expr e
  | Block stmts -> (* what's a sensible return value? *) 
      let ans = List.map codegen_stmt stmts in 
      List.hd (List.rev ans)
      (* for now I'm just passing back the last llvalue *)
  | Skip -> unimplemented (* ?? *)

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
  let decl = declare_function "id" (function_type brawn_type [|brawn_type|]) program_module in
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
  dump_module program_module

let _ = emit_llvm ()