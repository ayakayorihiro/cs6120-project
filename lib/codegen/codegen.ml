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

let rec codegen_if cond ts fs =
    (* condition to perform jump with *)
    let c' = build_runtime_call "is_true" [|codegen_expr cond|] in

    (* get the current block and function *)
    let curr = insertion_block builder in
    let fn = block_parent curr in

    (* emit true block *)
    let tb = append_block context "then" fn in
    position_at_end tb builder;
    codegen_stmt ts;
    let tb' = insertion_block builder in

    (* emit false block *)
    let fb = append_block context "else" fn in
    position_at_end fb builder;
    codegen_stmt fs;
    let fb' = insertion_block builder in

    (* emit merge block *)
    let cb = append_block context "cont" fn in
    position_at_end cb builder;

    (* add the conditional branch to the start block *)
    position_at_end curr builder;
    ignore (build_cond_br c' tb fb builder);

    (* jump to merge block *)
    position_at_end tb' builder; ignore (build_br cb builder);
    position_at_end fb' builder; ignore (build_br cb builder);
    position_at_end cb builder;

and codegen_for init guard update body =
  (* Emit the start code first *)
  (* let start_val = codegen_stmt init in

  (* Make the new basic block for the loop header, inserting after current block. *)
  let preheader_bb = insertion_block builder in
  let the_function = block_parent preheader_bb in
  let loop_bb = append_block context "loop" the_function in

  (* Insert an explicit fall through from the current block to the loop_bb. *)
  ignore (build_br loop_bb builder);

  (* Start insertion in loop_bb. *)
  position_at_end loop_bb builder;

  (* Start the PHI node with an entry for start. *)
  let variable = build_phi [(start_val, preheader_bb)] "tmp" builder in *)

  (* hmm let's do this together carefully *)
  unimplemented

and codegen_stmt = function
    | If (cond, ts, Some fs) -> codegen_if cond ts fs
    | If (cond, ts, None) -> codegen_stmt (If (cond, ts, Some Skip))
    | While (guard, body) -> unimplemented
    | Do (body, guard) -> unimplemented
    | For (is, c, us, s) ->
        let is' = if Option.is_some is then Option.get is else Skip in
        let c' = if Option.is_some c then Option.get c else Literal (Number 1.) in
        let us' = if Option.is_some us then Option.get us else Skip in
        codegen_for is' c' us' s
    | RangedFor (e, a, s) -> unimplemented
    | Break
    | Continue
    | Next -> unimplemented
    | Exit (Some e) -> unimplemented
    | Exit None -> unimplemented
    | Return (Some e) -> let e' = codegen_expr e in ignore (build_ret e' builder)
    | Return None -> ignore (build_ret_void builder)
    | Delete (i, es) ->
        let e' = codegen_expr (concat_array_index es) in
        ignore (build_runtime_call "delete_array" [|lookup_variable i; e'|])
    | Print [] -> codegen_stmt (Print [LValue (Dollar (Literal (Number 0.)))])
    | Print es ->
        let args = Array.of_list (List.map codegen_expr es) in
        let num = const_int (i32_type context) (List.length es) in
        ignore (build_runtime_call "print" (Array.append [|num|] args))
    | Expression e -> ignore (codegen_expr e)
    | Block stmts ->
        List.iter codegen_stmt stmts
    | Skip -> ()

let codegen_action = function
  | Action (None, None) -> raise (CodeGenError "brawn: action with no pattern or statement.")
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
