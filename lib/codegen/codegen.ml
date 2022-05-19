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
let brawn_null = const_null brawn_type

(* A hashtable for all the global variables *)
let global_variables = Hashtbl.create 10

(* A hashtable for all the local variables *)
let local_variables = Hashtbl.create 10

(* A hashtable for all the global constants *)
let global_constants = Hashtbl.create 10

(* A hashtable for all the functions *)
let global_functions = Hashtbl.create 10

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
    build_call func args ("call_temp") builder

(* todo eventually remove *)
let unimplemented = raise (CodeGenError "brawn: unimplemented.")

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
    | BinaryOp (Match, u, Literal (Regexp r)) -> codegen_binary Match (codegen_expr u) (lookup_constant (Regexp r))
    | BinaryOp (NonMatch, u, Literal (Regexp r)) -> codegen_binary NonMatch (codegen_expr u) (lookup_constant (Regexp r))
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
    | Literal (Regexp r) -> codegen_expr (BinaryOp (Match, LValue (Dollar (Literal (Number 0.))), Literal (Regexp r)))
    | Literal l -> lookup_constant l

and codegen_func_call func args =
    let Identifier func' = func in
    let args' = match (func, args) with
        | (Identifier "srand", []) -> [brawn_null]
        | (Identifier "length", []) -> [codegen_expr (LValue (Dollar (Literal (Number 0.))))]
        | (Identifier "gsub", [x; y])
        | (Identifier "sub", [x; y]) -> [codegen_expr x; codegen_expr y; codegen_expr (LValue (Dollar (Literal (Number 0.))))]
        | (Identifier "split", [x; y])
        | (Identifier "substr", [x; y]) -> [codegen_expr x; codegen_expr y; brawn_null]
        | _ -> List.map codegen_expr args
    in
    if Hashtbl.mem global_functions func
    then build_call (Hashtbl.find global_functions func) (Array.of_list args') "call_temp" builder
    else build_runtime_call func' (Array.of_list args')

let rec codegen_if eb cb c ts fs =
    (* get the current block and function *)
    let curr = insertion_block builder in
    let fn = block_parent curr in

    (* create the then, else, and merge blocks *)
    let tb = append_block context "true" fn in
    let fb = append_block context "false" fn in
    let mb = append_block context "merge" fn in

    (* condition to perform jump with *)
    position_at_end curr builder;
    let c' = build_runtime_call "is_true" [|codegen_expr c|] in
    ignore (build_cond_br c' tb fb builder);

    (* build true block *)
    position_at_end tb builder;
    codegen_stmt eb cb ts;
    ignore (build_br mb builder);

    (* build false block *)
    position_at_end fb builder;
    codegen_stmt eb cb fs;
    ignore (build_br mb builder);

    (* position at the merge block *)
    position_at_end mb builder;

and codegen_while c s =
    (* get the current block and function *)
    let curr = insertion_block builder in
    let fn = block_parent curr in

    (* create the loop header, body, and continue blocks *)
    let hb = append_block context "header" fn in
    let bb = append_block context "body" fn in
    let mb = append_block context "merge" fn in

    (* add the initial jump *)
    position_at_end curr builder;
    ignore (build_br hb builder);

    (* condition to perform jump with *)
    position_at_end hb builder;
    let c' = build_runtime_call "is_true" [|codegen_expr c|] in
    ignore (build_cond_br c' bb mb builder);

    (* build body block *)
    position_at_end bb builder;
    codegen_stmt mb hb s;
    ignore (build_br hb builder);

    (* position at the merge block *)
    position_at_end mb builder;

and codegen_for is c us s =
    (* get the current block and function *)
    let curr = insertion_block builder in
    let fn = block_parent curr in

    (* create the loop header, body, and continue blocks *)
    let hb = append_block context "header" fn in
    let bb = append_block context "body" fn in
    let mb = append_block context "merge" fn in

    (* build the preheader *)
    position_at_end curr builder;
    codegen_stmt mb hb is;
    ignore (build_br hb builder);

    (* condition to perform jump with *)
    position_at_end hb builder;
    let c' = build_runtime_call "is_true" [|codegen_expr c|] in
    ignore (build_cond_br c' bb mb builder);

    (* build body block *)
    position_at_end bb builder;
    codegen_stmt mb hb s;
    codegen_stmt mb hb us;
    ignore (build_br hb builder);

    (* position at the merge block *)
    position_at_end mb builder;

and codegen_range e a s =
    (* get the values for the array and element *)
    let e' = lookup_variable e in
    let a' = lookup_variable a in

    (* get the current block and function *)
    let curr = insertion_block builder in
    let fn = block_parent curr in

    (* create the loop header, body, and continue blocks *)
    let hb = append_block context "header" fn in
    let bb = append_block context "body" fn in
    let mb = append_block context "merge" fn in

    (* build the preheader *)
    position_at_end curr builder;
    let iter = build_runtime_call "init_iterator" [|a'|] in
    ignore (build_br hb builder);

    (* condition to perform jump with *)
    position_at_end hb builder;
    let v' = build_runtime_call "next_iterator" [|iter|] in
    let c' = build_icmp Icmp.Ne v' brawn_null "null_temp" builder in
    ignore (build_cond_br c' bb mb builder);

    (* build body block *)
    position_at_end bb builder;
    ignore (build_store e' v' builder);
    codegen_stmt mb hb s;
    ignore (build_br hb builder);

    (* position at the merge block *)
    position_at_end mb builder;

and codegen_stmt eb cb = function
    | If (cond, ts, Some fs) -> codegen_if eb cb cond ts fs
    | If (cond, ts, None) -> codegen_stmt eb cb (If (cond, ts, Some Skip))
    | While (c, s) -> codegen_while c s
    | Do (s, c) -> codegen_stmt eb cb (Block [s; While (c, s)])
    | For (is, c, us, s) ->
        let c' = Option.value c ~default:(Literal (Number 1.)) in
        let is' = Option.value is ~default:Skip in
        let us' = Option.value us ~default:Skip in
        codegen_for is' c' us' s
    | RangedFor (e, a, s) -> codegen_range e a s
    | Break -> ignore (build_br eb builder)
    | Continue -> ignore (build_br cb builder)
    | Next -> ignore (build_runtime_call "next" [||])
    | Exit (Some e) -> ignore (build_runtime_call "exit" [|codegen_expr e|])
    | Exit None -> ignore (build_runtime_call "exit" [|brawn_null|])
    | Return (Some e) -> let e' = codegen_expr e in ignore (build_ret e' builder)
    | Return None -> ignore (build_ret_void builder)
    | Delete (i, es) ->
        let e' = codegen_expr (concat_array_index es) in
        ignore (build_runtime_call "delete_array" [|lookup_variable i; e'|])
    | Print [] -> codegen_stmt eb cb (Print [LValue (Dollar (Literal (Number 0.)))])
    | Print es ->
        let args = Array.of_list (List.map codegen_expr es) in
        let num = const_int (i32_type context) (List.length es) in
        ignore (build_runtime_call "print" (Array.append [|num|] args))
    | Expression e -> ignore (codegen_expr e)
    | Block stmts ->
        List.iter (codegen_stmt eb cb) stmts
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
