open Ast


let id_to_str (Identifier s) = s

let expr_to_str_list _ = []
let expr_opt_to_str_list _ = []

let print_to_str_list _ = []

let rec stmt_opt_to_str_list _ = []
and
stmt_to_str_list = function
| If (expr, stmt1, stmt2) -> 
    "begin if" :: 
      "begin guard" ::
        expr_to_str_list expr @
      "end guard" :: 
      "begin if-branch" :: 
        stmt_to_str_list stmt1 @
      "end if-branch" :: 
      "begin else-branch" ::
        stmt_opt_to_str_list stmt2 @
      "end else-branch" :: 
    ["end if"]
| While (expr, stmt) ->
    "begin while" ::
      "begin guard" ::
        expr_to_str_list expr @ 
      "end guard" ::
      "begin body" ::
        stmt_to_str_list stmt @ 
      "end body" ::
    ["end while"]
| Do (stmt, expr) -> 
    "begin do" :: 
      "begin body" ::
        stmt_to_str_list stmt @ 
      "end body" ::
      "begin guard" ::
        expr_to_str_list expr @ 
      "end guard" ::
    ["end do"] 
| For (stmt_opt1, expr_opt, stmt_opt2, stmt) -> 
    "begin for" ::
      "begin for-init" :: 
        stmt_opt_to_str_list stmt_opt1 @ 
      "end for-init" ::
      "begin for-guard" :: 
        expr_opt_to_str_list expr_opt @ 
      "end for-guard" ::
      "begin for-update" :: 
        stmt_opt_to_str_list stmt_opt2 @ 
      "end for-guard" ::
      "begin body" ::
         stmt_to_str_list stmt @ 
      "end body" ::
    ["end for"]
| RangedFor (name1, name2, stmt) ->
    "begin rangedfor" :: 
      id_to_str name1 :: 
      id_to_str name2 :: 
      stmt_to_str_list stmt @ 
    ["end rangedfor"]
| Break -> ["break"]
| Continue -> ["continue"]
| Next -> ["next"]
| Exit expr_opt -> 
    "begin exit" :: 
      expr_opt_to_str_list expr_opt @ 
    ["end exit"]
| Return expr_opt -> 
    "begin return" :: 
      expr_opt_to_str_list expr_opt @ 
    ["end return"]
| Delete (ident, exprs) ->
    "begin delete" :: 
      id_to_str ident ::
      List.concat_map expr_to_str_list exprs @ 
    ["end delete"]
| Print print -> 
    "begin print" ::
      print_to_str_list print @ 
    ["end print"]
| Expression expr -> 
    "begin expr" ::
      (expr_to_str_list expr) @ 
    ["end expr"]
| Block stmts -> 
    "begin block" ::  
      (List.concat_map stmt_to_str_list stmts) @ 
    ["end block"]
| Skip -> ["skip"]

let serialize_func_defn (Function (name, args, statements)) = 
  ("funcdefn", [id_to_str name]) :: 
  ["args", (List.map id_to_str args)] @
  ["statements", List.concat_map stmt_to_str_list statements]

let serialize_action_decl patterns statements = []

let serialize_item item = 
  match item with
  | FunctionDecl f -> serialize_func_defn f 
  | ActionDecl (patterns, statements) -> serialize_action_decl patterns statements

let serialize_prog (Program items) = List.concat_map serialize_item items