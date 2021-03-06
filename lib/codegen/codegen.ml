open Llvm
open Brawn_ast.Ast
open Brawn_typecheck.Builtins
open Brawn_typecheck.Typecheck
    
(* Exception emitted by codegen fuctions *)
exception CodeGenError of string

(* Llvm constants *)
let runtime_file = try Sys.argv.(1) with _ -> print_endline "usage: brawnc runtime_module.ll input_file.brawn output_file.ll"; exit 1
let context = create_context ()
let runtime_module = Llvm_irreader.parse_ir context (Llvm.MemoryBuffer.of_file runtime_file)
let program_module = create_module context "brawn"
let builder = builder context

(* Llvm types *)
let brawn_value_type = Option.get (type_by_name runtime_module "struct.brawn::brawn_value")
let regex_value_type = Option.get (type_by_name runtime_module "class.std::__1::basic_regex")
let brawn_type = pointer_type brawn_value_type
let regex_type = pointer_type regex_value_type
let regex_null = const_null regex_type
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
let dollar_0 = LValue (Dollar (Literal (String "0")))

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
        | Subtract -> "subtr"
        | Multiply -> "mult"
        | Concat -> "concat"
        | Equals -> "eq"
        | NotEquals -> "ne"
        | LessThan -> "lt"
        | LessThanEq -> "le"
        | GreaterThan -> "gt"
        | GreaterThanEq -> "ge"
        | Match -> (if type_of v = brawn_type then "match_builtin" else "match_builtin_regex")
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
        build_runtime_call "update_array" [|v'; l'; u|]

and codegen_expr = function
    | BinaryOp (Match, u, Literal (Regexp r)) ->
        let l = lookup_constant (Regexp r) in
        let l' = build_load l "load_temp" builder in
        codegen_binary Match (codegen_expr u) l'
    | BinaryOp (NonMatch, u, Literal (Regexp r)) ->
        let l = lookup_constant (Regexp r) in
        let l' = build_load l "load_temp" builder in
        codegen_binary NonMatch (codegen_expr u) l'
    | BinaryOp (op, u, v) -> codegen_binary op (codegen_expr u) (codegen_expr v)
    | UnaryOp (op, u) -> codegen_unary op (codegen_expr u)
    | UpdateOp (op, l, u) -> codegen_expr (Assignment (l, BinaryOp (get_op_from_updateop op, LValue l, u)))
    | Getline None -> build_runtime_call "getline" [|brawn_null|]
    | Getline (Some l) -> build_runtime_call "getline" [|codegen_lvalue_get l|]
    | FuncCall (Identifier fn, ag) -> codegen_func_call fn ag
    | Ternary (u, v, w) ->
        let u' = codegen_expr u in
        let v' = codegen_expr v in
        let w' = codegen_expr w in
        let cond = build_runtime_call "is_true" [|u'|] in
        build_select cond v' w' "ternary_temp" builder
    | Member (es, l) ->
        let e' = codegen_expr (concat_array_index es) in
        let a' = codegen_lvalue_get (IdentVal l) in
        build_runtime_call "member" [|e'; a'|]
    | Postfix (Incr, l) ->
        let o' = codegen_lvalue_get l in
        let n' = build_runtime_call "add" [|o'; codegen_expr (Literal (Number 1.))|] in
        ignore (codegen_lvalue_update n' l); o'
    | Postfix (Decr, l) ->
        let o' = codegen_lvalue_get l in
        let n' = build_runtime_call "add" [|o'; codegen_expr (Literal (Number 1.))|] in
        ignore (codegen_lvalue_update n' l); o'
    | Prefix (Incr, l) -> codegen_expr (Assignment (l, BinaryOp (Plus, LValue l, Literal (Number 1.))))
    | Prefix (Decr, l) -> codegen_expr (Assignment (l, BinaryOp (Subtract, LValue l, Literal (Number 1.))))
    | LValue l -> codegen_lvalue_get l
    | Assignment (l, u) ->
        let u' = codegen_expr u in
        ignore (codegen_lvalue_update u' l);
        u'
    | Literal (Regexp r) -> codegen_expr (BinaryOp (Match, dollar_0, Literal (Regexp r)))
    | Literal l ->
        let v = lookup_constant l in
        build_load v "load_temp" builder

and codegen_missing_args = function
    | 0 -> []
    | num -> (build_runtime_call "init" [||]) :: (codegen_missing_args (num - 1))

and codegen_func_call fn ag =
    let (fn', ag') = match (fn, ag) with
        | ("srand", []) -> (fn, [brawn_null])
        | ("length", []) -> (fn, [brawn_null])
        | ("gsub", [Literal (Regexp r); y; z])
        | ("sub", [Literal (Regexp r); y; z]) -> (fn ^ "_regex", [build_load (lookup_constant (Regexp r)) "load_temp" builder; codegen_expr y; codegen_expr z])
        | ("gsub", [Literal (Regexp r); y])
        | ("sub", [Literal (Regexp r); y]) -> (fn ^ "_regex", [build_load (lookup_constant (Regexp r)) "load_temp" builder; codegen_expr y; brawn_null])
        | ("gsub", [x; y])
        | ("sub", [x; y]) -> (fn, [codegen_expr x; codegen_expr y; brawn_null])
        | ("split", [x; y])
        | ("substr", [x; y]) -> (fn, [codegen_expr x; codegen_expr y; brawn_null])
        | ("split", [x; y; Literal (Regexp r)]) -> (fn ^ "_regex", [codegen_expr x; codegen_expr y; build_load (lookup_constant (Regexp r)) "load_temp" builder])
        | ("match", [x; Literal (Regexp r)]) -> (fn ^ "_regex", [codegen_expr x;  build_load (lookup_constant (Regexp r)) "load_temp" builder])
        | _ -> (fn, List.map codegen_expr ag)
    in
    if List.mem_assq fn builtin_functions && List.length ag' < List.assq fn builtin_functions
    then raise (CodeGenError "brawn: invalid number of argments for function.")
    else ();
    if Hashtbl.mem defined_functions fn
    then
        let f = Hashtbl.find defined_functions fn in
        let missing = (Array.length (param_types (element_type (type_of f)))) - (List.length ag') in
        let args = Array.of_list (ag' @ (codegen_missing_args missing)) in
        build_call f args "call_temp" builder
    else build_runtime_call fn' (Array.of_list ag')

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
    let tb' = insertion_block builder in
    if Option.is_none (block_terminator tb') then ignore (build_br mb builder);

    (* build false block *)
    position_at_end fb builder;
    codegen_stmt eb cb fs;
    let fb' = insertion_block builder in
    if Option.is_none (block_terminator fb') then ignore (build_br mb builder);

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
    let bb' = insertion_block builder in
    if Option.is_none (block_terminator bb') then ignore (build_br hb builder);

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
    let bb' = insertion_block builder in
    if Option.is_none (block_terminator bb') then ignore (build_br hb builder);

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
    let bb' = insertion_block builder in
    if Option.is_none (block_terminator bb') then ignore (build_br hb builder);

    (* position at the merge block *)
    position_at_end mb builder;

and codegen_jump b =
    (* get the current block and function and put a break there *)
    let curr = insertion_block builder in
    let fn = block_parent curr in
    ignore (build_br b builder);

    (* create the next block *)
    let nb = append_block context "break" fn in
    position_at_end nb builder

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
    | Break -> codegen_jump (Option.get eb)
    | Continue -> codegen_jump (Option.get cb)
    | Next -> ignore (build_runtime_call "next" [||])
    | Exit (Some e) -> ignore (build_runtime_call "exit" [|codegen_expr e|])
    | Exit None -> ignore (build_runtime_call "exit" [|brawn_null|])
    | Return (Some e) -> let e' = codegen_expr e in ignore (build_ret e' builder)
    | Return None -> ignore (build_ret (build_runtime_call "init" [||]) builder)
    | Delete (a, es) ->
        let e' = codegen_expr (concat_array_index es) in
        let a' = codegen_lvalue_get (IdentVal a) in
        ignore (build_runtime_call "delete_array" [|a'; e'|])
    | Print [] -> codegen_stmt eb cb (Print [dollar_0])
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
    Hashtbl.add local_variables a v

let codegen_func (Function (Identifier n, ag, b)) =
    (* clear the local variable tables *)
    Hashtbl.clear local_variables;
    let fn = codegen_func_proto n (List.length ag) in
    let bb = append_block context "entry" fn in
    position_at_end bb builder;

    (* repopulate the local variable tables *)
    List.iter codegen_arg ag;
    codegen_stmt None None b;
    let b = insertion_block builder in
    if Option.is_none (block_terminator b)
    then ignore (build_ret (build_runtime_call "init" [||]) builder);
    Llvm_analysis.assert_valid_function fn

let codegen_brawn name b =
    (* clear the local variable tables *)
    Hashtbl.clear local_variables;
    let fn = declare_function ("brawn_" ^ name) (function_type void [||]) program_module in
    let bb = append_block context "entry" fn in
    position_at_end bb builder;
    codegen_stmt None None b;
    ignore (build_ret_void builder);
    Llvm_analysis.assert_valid_function fn

let end_action = function
    | Action (Some End, Some s) -> [s]
    | _ -> []
let begin_action = function
    | Action (Some Begin, Some s) -> [s]
    | _ -> []

let codegen_begin_end acs =
    let eb = Block (List.concat_map end_action acs) in
    codegen_brawn "end" eb;
    let bb = Block (List.concat_map begin_action acs) in
    codegen_brawn "begin" bb

let codegen_func_decl (Function (Identifier fn, ag, _)) =
    let f = codegen_func_proto fn (List.length ag) in
    set_visibility Visibility.Hidden f;
    set_linkage Linkage.Internal f

let codegen_global i _ =
    let g = if not (List.mem i builtin_variables)
            then let g = define_global i brawn_null program_module in
                 set_visibility Visibility.Hidden g;
                 set_linkage Linkage.Internal g; g
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
                    define_global "regex" regex_null program_module
    in
    set_visibility Visibility.Hidden v;
    set_linkage Linkage.Internal v;
    Hashtbl.add global_constants l v

(* Generate code for global initialisation functions. *)
let codegen_init_func () =
    let f = declare_function "brawn_ctor" (function_type void [||]) program_module in
    set_visibility Visibility.Hidden f;
    set_linkage Linkage.Internal f;
    let b = append_block context "entry" f in
    position_at_end b builder;
    let process_literal l v =
        let c = match l with
            | Number n -> build_runtime_call "from_number" [|const_float (double_type context) n|]
            | String s ->
                let c = const_stringz context s in
                let g = define_global "" c program_module in
                set_linkage Linkage.Private g;
                set_unnamed_addr true g;
                set_global_constant true g;
                let i = const_int (i64_type context) 0 in
                let v = const_in_bounds_gep g [|i; i|] in
                build_runtime_call "from_const_string" [|v|]
            | Regexp r ->
                let c = const_stringz context r in
                let g = define_global "" c program_module in
                set_linkage Linkage.Private g;
                set_unnamed_addr true g;
                set_global_constant true g;
                let i = const_int (i64_type context) 0 in
                let v = const_in_bounds_gep g [|i; i|] in
                build_runtime_call "init_regex" [|v|]
        in ignore (build_store c v builder) in
    let process_global g v =
        if not (List.mem g builtin_variables) then
        ignore (build_store (build_runtime_call "init" [||]) v builder) in
    Hashtbl.iter process_global global_variables;
    Hashtbl.iter process_literal global_constants;
    ignore (build_ret_void builder);
    Llvm_analysis.assert_valid_function f;

    (* add this function to the const llvm.global_ctors *)
    let t = struct_type context [|i32_type context; pointer_type (function_type void [||]); pointer_type (i8_type context)|] in
    let v = const_struct context [|const_int (i32_type context) 65534; f; const_pointer_null (pointer_type ((i8_type context)))|] in
    let g = define_global "llvm.global_ctors" (const_array t [|v|]) program_module in
    set_linkage Linkage.Appending g

(* Generate code for all the runtime functions. *)
let codegen_runtime_funcs () =
    let process f =
        let name = value_name f in
        if (String.length name > 6 && (String.sub name 0 6) = "brawn_") then
        ignore (declare_function (value_name f) (element_type (type_of f)) program_module) in
    iter_functions process runtime_module

(* Generate code for all the global variables. *)
let codegen_globals () =
    Hashtbl.iter codegen_global globals

(* Generate code for all the literal constants. *)
let codegen_literals () =
    Hashtbl.iter codegen_literal constants

(* Generate code for main action logic. *)
let codegen_action acs =
    let process = function
        | Action (None, None)
        | Action (Some Begin, _)
        | Action (Some End, _) -> []
        | Action (None, Some s) -> [s]
        | Action (Some (Expr e), None) -> [Expression e]
        | Action (Some (Expr e), Some s) -> [If (e, s, None)] in
    let ss = List.concat_map process acs in
    codegen_brawn "process" (Block ss)

(* Generate code for the given awk program. *)
let codegen_program (Program (fs, acs)) =
    set_target_triple (target_triple runtime_module) program_module;
    set_data_layout (data_layout runtime_module) program_module;
    codegen_runtime_funcs ();
    codegen_globals ();
    codegen_literals ();
    codegen_init_func ();
    List.iter codegen_func_decl fs;
    List.iter codegen_func fs;
    codegen_begin_end acs;
    codegen_action acs
