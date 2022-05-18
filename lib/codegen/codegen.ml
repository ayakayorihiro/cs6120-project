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

(* A hashtable for all the global constants *)
let global_constants = Hashtbl.create 10

(* A hashtable for all the local variables *)
let local_variables = Hashtbl.create 10

(* The $ global variable. *)
let dollar_value = Option.get (lookup_global "DOLLAR" runtime_module)

(* Lookup an identifier. *)
let lookup_variable name =
    if Hashtbl.mem local_variables name
    then Hashtbl.find local_variables name
    else if Hashtbl.mem global_variables name
    then Hashtbl.find global_variables name
    else raise (CodeGenError "brawn: unknown identifier.")

(* Declare all runtime functions. *)
let build_runtime_call name args =
    let func = Option.get (lookup_function ("brawn_" ^ name) runtime_module) in
    build_call func args (name ^ "_temp") builder

(* todo eventually remove *)
let unimplemented = const_string context "unimplemented"

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

let rec codegen_expr = function
  | BinaryOp (op, u, v) -> codegen_binary op (codegen_expr u) (codegen_expr v)
  | UnaryOp (op, u) -> codegen_unary op (codegen_expr u)
  | UpdateOp (op, l, u) -> codegen_expr (Assignment (l, BinaryOp (get_op_from_updateop op, LValue l, u)))
  | Getline None -> unimplemented
  | Getline (Some l) -> unimplemented
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
  | Postfix (Incr, l) -> unimplemented
  | Postfix (Decr, l) -> unimplemented
  | Prefix (Incr, l) -> codegen_expr (Assignment (l, BinaryOp (Plus, LValue l, Literal (Number 1.))))
  | Prefix (Decr, l) -> codegen_expr (Assignment (l, BinaryOp (Subtract, LValue l, Literal (Number 1.))))
  | LValue (IdentVal i) -> build_load (lookup_variable i) "load_temp" builder
  | LValue (ArrayVal (i, es)) ->
      let a' = lookup_variable i in
      let l' = codegen_expr (concat_array_index es) in
      build_runtime_call "index_array" [|a'; l'|]
  | LValue (Dollar e) ->
      let l' = codegen_expr e in
      build_runtime_call "index_array" [|dollar_value; l'|]
  | Assignment (IdentVal i, u) ->
      let u' = codegen_expr u in
      build_store (lookup_variable i) u' builder
  | Assignment (ArrayVal (i, es), u) ->
      let a' = lookup_variable i in
      let l' = codegen_expr (concat_array_index es) in
      let u' = codegen_expr u in
      build_runtime_call "update_array" [|a'; l'; u'|]
  | Assignment (Dollar e, u) ->
      let l' = codegen_expr e in
      let u' = codegen_expr u in
      build_runtime_call "update_array" [|dollar_value; l'; u'|]
  | Literal l -> if Hashtbl.mem global_constants l
                 then Hashtbl.find global_constants l
                 else raise (CodeGenError "brawn: literal constant not found.")

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
