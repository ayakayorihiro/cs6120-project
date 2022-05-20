open Llvm
open Brawn_ast.Ast
open Brawn_typecheck.Builtins

exception CodeGenError of string

(* Llvm constants *)
let context = create_context ()
let runtime_module = Llvm_irreader.parse_ir context (Llvm.MemoryBuffer.of_file "/Users/shubham/Documents/Code/cs6120/project/brawn/runtime/brawn_runtime.ll")
let program_module = create_module context "brawn"
let builder = builder context

(* Llvm types *)
let brawn_value_type = Option.get (type_by_name runtime_module "struct.brawn::brawn_value")
let regex_value_type = Option.get (type_by_name runtime_module "class.std::__1::basic_regex")
let brawn_type = pointer_type brawn_value_type
let regex_type = pointer_type regex_value_type
let brawn_null = const_null brawn_type
let void = void_type context

(* A hashtable for all the global variables *)
let global_variables = Hashtbl.create 10

(* A hashtable for all the local variables *)
let local_variables = Hashtbl.create 10

(* A hashtable for all the global constants *)
let global_constants = Hashtbl.create 10

(* A hashtable for all the functions *)
let defined_functions = Hashtbl.create 10

(* The $ global variable. *)
let dollar_value = declare_global brawn_type "DOLLAR" program_module

(* Lookup an identifier. *)
let lookup_variable name =
    if Hashtbl.mem local_variables name
    then Hashtbl.find local_variables name
    else if Hashtbl.mem global_variables name
    then Hashtbl.find global_variables name
    else raise (CodeGenError ("brawn: unknown identifier " ^ name ^ "."))

(* Lookup a global constant. *)
let lookup_constant literal =
    if Hashtbl.mem global_constants literal
    then Hashtbl.find global_constants literal
    else raise (CodeGenError "brawn: unknown literal.")

(* Declare all runtime functions. *)
let build_runtime_call name args =
    let fn = Option.get (lookup_function ("brawn_" ^ name) program_module) in
    build_call fn args "call_temp" builder

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
    | IdentVal (Identifier i) ->
        let v = lookup_variable i in
        build_load v "load_temp" builder
    | ArrayVal (Identifier i, es) ->
        let v = lookup_variable i in
        let v' = build_load v "load_temp" builder in
        let l' = codegen_expr (concat_array_index es) in
        build_runtime_call "index_array" [|v'; l'|]
    | Dollar e ->
        let l' = codegen_expr e in
        let v' = build_load dollar_value "load_temp" builder in
        build_runtime_call "index_array" [|v'; l'|]

and codegen_lvalue_update u = function
    | IdentVal (Identifier i) ->
        let v = lookup_variable i in
        build_store u v builder
    | ArrayVal (Identifier i, es) ->
        let a = lookup_variable i in
        let v' = build_load a "load_temp" builder in
        let l' = codegen_expr (concat_array_index es) in
        build_runtime_call "update_array" [|v'; l'; u|]
    | Dollar e ->
        let v' = build_load dollar_value "load_temp" builder in
        let l' = codegen_expr e in
        build_runtime_call "index_array" [|v'; l'|]

and codegen_expr = function
    | BinaryOp (Match, u, Literal (Regexp r)) -> codegen_binary Match (codegen_expr u) (lookup_constant (Regexp r))
    | BinaryOp (NonMatch, u, Literal (Regexp r)) -> codegen_binary NonMatch (codegen_expr u) (lookup_constant (Regexp r))
    | BinaryOp (op, u, v) -> codegen_binary op (codegen_expr u) (codegen_expr v)
    | UnaryOp (op, u) -> codegen_unary op (codegen_expr u)
    | UpdateOp (op, l, u) -> codegen_expr (Assignment (l, BinaryOp (get_op_from_updateop op, LValue l, u)))
    | Getline None -> codegen_expr (Getline (Some (Dollar (Literal (Number 0.)))))
    | Getline (Some l) -> build_runtime_call "getline" [|codegen_lvalue_get l|]
    | FuncCall (Identifier fn, ag) -> codegen_func_call fn ag
    | Ternary (u, v, w) ->
        let u' = codegen_expr u in
        let v' = codegen_expr v in
        let w' = codegen_expr w in
        let cond = build_runtime_call "is_true" [|u'|] in
        build_select cond v' w' "ternary_temp" builder
    | Member (es, Identifier i) ->
        let e' = codegen_expr (concat_array_index es) in
        build_runtime_call "member" [|e'; lookup_variable i|]
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
    | Literal l ->
        let v = lookup_constant l in
        build_load v "load_temp" builder

and codegen_missing_args = function
    | 0 -> []
    | num -> (build_runtime_call "init" [||]) :: codegen_missing_args (num - 1)

and codegen_func_call fn ag =
    let ag' = match (fn, ag) with
        | ("srand", []) -> [brawn_null]
        | ("length", []) -> [codegen_expr (LValue (Dollar (Literal (Number 0.))))]
        | ("gsub", [x; y])
        | ("sub", [x; y]) -> [codegen_expr x; codegen_expr y; codegen_expr (LValue (Dollar (Literal (Number 0.))))]
        | ("split", [x; y])
        | ("substr", [x; y]) -> [codegen_expr x; codegen_expr y; brawn_null]
        | _ -> List.map codegen_expr ag
    in
    if List.mem_assq fn builtin_functions && List.length ag' < List.assq fn builtin_functions
    then raise (CodeGenError "brawn: invalid number of argments for function.")
    else ();
    if Hashtbl.mem defined_functions fn
    then
        let f = Hashtbl.find defined_functions fn in
        let missing = (Array.length (param_types (type_of f))) - (List.length ag') in
        let args = Array.of_list (ag' @ (codegen_missing_args missing)) in
        build_call f args "call_temp" builder
    else build_runtime_call fn (Array.of_list ag')

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
    codegen_stmt (Some mb) (Some hb) s;
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
    codegen_stmt (Some mb) (Some hb) is;
    ignore (build_br hb builder);

    (* condition to perform jump with *)
    position_at_end hb builder;
    let c' = build_runtime_call "is_true" [|codegen_expr c|] in
    ignore (build_cond_br c' bb mb builder);

    (* build body block *)
    position_at_end bb builder;
    codegen_stmt (Some mb) (Some hb) s;
    codegen_stmt (Some mb) (Some hb) us;
    ignore (build_br hb builder);

    (* position at the merge block *)
    position_at_end mb builder;

and codegen_range e a s =
    (* get the values for the array and element *)
    let e' = lookup_variable e in
    let v = lookup_variable a in

    (* get the current block and function *)
    let curr = insertion_block builder in
    let fn = block_parent curr in

    (* create the loop header, body, and continue blocks *)
    let hb = append_block context "header" fn in
    let bb = append_block context "body" fn in
    let mb = append_block context "merge" fn in

    (* build the preheader *)
    position_at_end curr builder;
    let a' = build_load v "load_temp" builder in
    let iter = build_runtime_call "init_iterator" [|a'|] in
    ignore (build_br hb builder);

    (* condition to perform jump with *)
    position_at_end hb builder;
    let v' = build_runtime_call "next_iterator" [|iter|] in
    let c' = build_icmp Icmp.Ne v' brawn_null "null_temp" builder in
    ignore (build_cond_br c' bb mb builder);

    (* build body block *)
    position_at_end bb builder;
    ignore (build_store v' e' builder);
    codegen_stmt (Some mb) (Some hb) s;
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
    | RangedFor (Identifier e, Identifier a, s) -> codegen_range e a s
    | Break -> ignore (build_br (Option.get eb) builder)
    | Continue -> ignore (build_br (Option.get cb) builder)
    | Next -> ignore (build_runtime_call "next" [||])
    | Exit (Some e) -> ignore (build_runtime_call "exit" [|codegen_expr e|])
    | Exit None -> ignore (build_runtime_call "exit" [|brawn_null|])
    | Return (Some e) -> let e' = codegen_expr e in ignore (build_ret e' builder)
    | Return None -> ignore (build_ret_void builder)
    | Delete (Identifier i, es) ->
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

let codegen_func_proto name num =
    let at = Array.make num brawn_type in
    let ft = function_type brawn_type at in
    if Hashtbl.mem defined_functions name
    then Hashtbl.find defined_functions name
    else let fn = declare_function name ft program_module in
         Hashtbl.add defined_functions name fn;
         fn

let codegen_arg (Identifier a) =
    let v = build_alloca brawn_type a builder in
    print_endline (string_of_int (alignment v));
    Hashtbl.add local_variables a v

let codegen_func (Function (Identifier n, ag, b)) =
    (* clear the local variable tables *)
    Hashtbl.clear local_variables;
    let fn = codegen_func_proto n (List.length ag) in
    let bb = append_block context "entry" fn in
    position_at_end bb builder;
    try
        (* repopulate the local variable tables *)
        List.iter codegen_arg ag;
        codegen_stmt None None b;
        ignore (build_ret brawn_null builder);
        Llvm_analysis.assert_valid_function fn;
    with e ->
        delete_function fn;
        raise e

let end_action = function
    | Action (Some End, Some s) -> [s]
    | _ -> []
let begin_action = function
    | Action (Some Begin, Some s) -> [s]
    | _ -> []

let codegen_begin_end acs =
    let eb = Block (List.concat_map end_action acs) in
    codegen_func (Function (Identifier "brawn_end", [], eb));
    let bb = Block (List.concat_map begin_action acs) in
    codegen_func (Function (Identifier "brawn_begin", [], bb))

let codegen_func_decl (Function (Identifier fn, ag, _)) = ignore (codegen_func_proto fn (List.length ag))

let codegen_global i _ =
    let g = if not (List.mem i builtin_variables)
            then define_global i brawn_null program_module
            else match (lookup_global i runtime_module) with
                | Some v -> declare_global brawn_type (value_name v) program_module
                | None -> raise (CodeGenError ("brawn: could not find name " ^ i ^ " in runtime."))
    in Hashtbl.add global_variables i g

let codegen_literal l _ =
    let v = match l with
                | Number _ ->
                    define_global "number" brawn_null program_module
                | String _ ->
                    define_global "string" brawn_null program_module
                | Regexp _ ->
                    define_global "regex" brawn_null program_module
    in
    Hashtbl.add global_constants l v

let codegen_runtime_funcs () =
    let process f =
        let name = value_name f in
        if (String.length name > 6 && (String.sub name 0 6) = "brawn_") then
        ignore (declare_function (value_name f) (element_type (type_of f)) program_module) in
    iter_functions process runtime_module

let codegen_program (Program (fs, acs)) =
    List.iter codegen_func_decl fs;
    List.iter codegen_func fs;
    codegen_begin_end acs;
