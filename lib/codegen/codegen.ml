open Llvm
open Brawn_ast.Ast

exception CodeGenError of string

(* Llvm constants *)
let context = create_context ()
let runtime_module = Llvm_irreader.parse_ir context (Llvm.MemoryBuffer.of_file "../runtime/brawn_runtime.ll")
let program_module = create_module context "brawn"
let builder = builder context

(* Llvm types *)
let brawn_value_type = Option.get (type_by_name runtime_module "struct::brawn.brawn_value")
let regex_value_type = Option.get (type_by_name runtime_module "class.std::__1::basic_regex")
let brawn_type = pointer_type brawn_value_type
let regex_type = pointer_type regex_value_type

(* A hashtable for all the global variables *)
let global_variables = Hashtbl.create 10

(* A hashtable for all the local variables *)
let local_variables = Hashtbl.create 10

(* A hashtable for all the global constants *)
let global_constants = Hashtbl.create 10

(* The $ global variable. *)
let dollar_value = Option.get (lookup_global "DOLLAR" runtime_module)

(* Lookup an identifier. *)
let lookup_variable name =
    if Hashtbl.mem local_variables name
    then Hashtbl.find local_variables name
    else if Hashtbl.mem global_variables name
    then Hashtbl.find global_variables name
    else raise (CodeGenError "brawn: unknown identifier.")

(* Lookup a global constant. *)
let lookup_constant literal =
    if Hashtbl.mem global_constants literal
    then Hashtbl.find global_constants literal
    else raise (CodeGenError "brawn: unknown literal.")

(* Declare all runtime functions. *)
let build_runtime_call name args =
    let func = Option.get (lookup_function ("brawn_" ^ name) runtime_module) in
    build_call func args (name ^ "_temp") builder

(* todo eventually remove *)
let unimplemented = raise (CodeGenError "brawn: unimplemented.")

let codegen_func_call func args = unimplemented

let codegen_binary op u v =
    let name = match op with
        | Or -> "or"
        | And -> "and"
        | Mod -> "mod"
        | Pow -> "pow"
        | Plus -> "add"
        | Divide -> "div"
        | Subtract -> "sub"
        | Multiply -> "mult"
        | Concat -> "concat"
        | Equals -> "eq"
        | NotEquals -> "ne"
        | LessThan -> "lt"
        | LessThanEq -> "le"
        | GreaterThan -> "gt"
        | GreaterThanEq -> "ge"
        | Match -> (if type_of v = brawn_type then "match" else "match_regex")
        | NonMatch -> (if type_of v = brawn_type then "not_match" else "non_match_regex") in
    build_runtime_call name [|u; v|]

let codegen_unary op u =
    let name = match op with
        | Negative -> "neg"
        | Positive -> "pos"
        | Not -> "not" in
    build_runtime_call name [|u|]

let rec concat_array_index = function
    | [] -> raise (CodeGenError "brawn: invalid subscript expression.")
    | [e] -> e
    | (e :: es) -> let subsep = LValue (IdentVal (Identifier "SUBSEP")) in
                   BinaryOp (Concat, e, BinaryOp (Concat, subsep, concat_array_index es))

let get_op_from_updateop = function
    | PowAssign -> Pow
    | ModAssign -> Mod
    | MulAssign -> Multiply
    | DivAssign -> Divide
    | AddAssign -> Plus
    | SubAssign -> Subtract

let rec codegen_lvalue_get = function
    | IdentVal i -> build_load (lookup_variable i) "load_temp" builder
    | ArrayVal (i, es) ->
        let a' = lookup_variable i in
        let l' = codegen_expr (concat_array_index es) in
        build_runtime_call "index_array" [|a'; l'|]
    | Dollar e ->
        let l' = codegen_expr e in
        build_runtime_call "index_array" [|dollar_value; l'|]

and codegen_lvalue_update u = function
    | IdentVal i -> build_store (lookup_variable i) u builder
    | ArrayVal (i, es) ->
        let a' = lookup_variable i in
        let l' = codegen_expr (concat_array_index es) in
        build_runtime_call "update_array" [|a'; l'; u|]
    | Dollar e ->
        let l' = codegen_expr e in
        build_runtime_call "update_array" [|dollar_value; l'; u|]

and codegen_expr = function
    | BinaryOp (op, u, v) -> codegen_binary op (codegen_expr u) (codegen_expr v)
    | UnaryOp (op, u) -> codegen_unary op (codegen_expr u)
    | UpdateOp (op, l, u) -> codegen_expr (Assignment (l, BinaryOp (get_op_from_updateop op, LValue l, u)))
    | Getline None -> codegen_expr (Getline (Some (Dollar (Literal (Number 0.)))))
    | Getline (Some l) -> build_runtime_call "getline" [|codegen_lvalue_get l|]
    | FuncCall (name, args) -> codegen_func_call name args
    | Ternary (u, v, w) ->
        let u' = codegen_expr u in
        let v' = codegen_expr v in
        let w' = codegen_expr w in
        let cond = build_runtime_call "is_true" [|u'|] in
        build_select cond v' w' "ternary_temp" builder
    | Member (es, name) ->
        let e' = codegen_expr (concat_array_index es) in
        build_runtime_call "member" [|e'; lookup_variable name|]
    | Postfix (Incr, l) ->
        let o' = codegen_lvalue_get l in
        let n' = build_runtime_call "add" [|o'; lookup_constant (Number 1.)|] in
        ignore (codegen_lvalue_update n' l); o'
    | Postfix (Decr, l) ->
        let o' = codegen_lvalue_get l in
        let n' = build_runtime_call "add" [|o'; lookup_constant (Number 1.)|] in
        ignore (codegen_lvalue_update n' l); o'
    | Prefix (Incr, l) -> codegen_expr (Assignment (l, BinaryOp (Plus, LValue l, Literal (Number 1.))))
    | Prefix (Decr, l) -> codegen_expr (Assignment (l, BinaryOp (Subtract, LValue l, Literal (Number 1.))))
    | LValue l -> codegen_lvalue_get l
    | Assignment (l, u) ->
        let u' = codegen_expr u in
        codegen_lvalue_update u' l
    | Literal l -> lookup_constant l

let rec codegen_stmt  = function
    | If (e, ts, Some fs) ->
        let c' = codegen_expr e in
        let this = insertion_block builder in
        let fn = block_parent this in
        let tru = append_block context "true" fn in
        position_at_end tru builder;
    | If (guard, then_stmt, None) -> unimplemented
    | While (guard, body) -> unimplemented
    | Do (body, guard) -> unimplemented
    | For (init_opt, guard_opt, update_opt, body) -> unimplemented
    | RangedFor (ele, array, body) -> unimplemented
    | Break
    | Continue
    | Next -> unimplemented
    | Exit (Some e) -> unimplemented
    | Exit None -> unimplemented
    | Return (Some e) ->
        let e' = codegen_expr e in
        ignore (build_ret e' builder)
    | Return None -> ignore (build_ret_void builder)
    | Delete (name, exprs) -> unimplemented
    | Print exprs -> unimplemented
    | Expression e -> ignore (codegen_expr e)
    | Block stmts ->
        List.iter codegen_stmt stmts
    | Skip -> ()

let codegen_action = function
  | Action (None, None) -> raise (CodeGenError "Can't get here")
  | Action (None, Some stmt) -> unimplemented
  | Action (Some patt, None) -> unimplemented
  | Action (Some patt, Some stmt) -> unimplemented

let codegen_func_proto name num_args =
  let args_types = Array.make num_args brawn_type in
  let ft = function_type brawn_type args_types in
  match lookup_function name program_module with
  | None -> declare_function name ft program_module
  | Some _ -> raise (CodeGenError "redefinition of function")
  (* I've simplified this compared to the guide. Redefinition is never allowed. *)

let codegen_func (Function (Identifier name, args, body)) =
  (* the guide says to clear out the named_values hashtable, I'm skipping that. *)
  let the_function = codegen_func_proto name (List.length args) in
  match body with
  | None -> the_function (* it was just a prototype. we're done. *)
  | Some body' -> (* it had a body. add it. *)
      let bb = append_block context "entry" the_function in
      position_at_end bb builder;
      try
        let ret_val = codegen_stmt body' in
        let _ = build_ret ret_val builder in
        Llvm_analysis.assert_valid_function the_function;
        the_function
      with e ->
        delete_function the_function;
        raise e

(* TODO: this should be fleshed out further to take care of control flow etc *)
let codegen_program ((Program (funcs, actions)) : program) =
  ignore (List.map codegen_func funcs);
  ignore (List.map codegen_action actions);
  dump_module program_module   (* more sensible return value? *)
