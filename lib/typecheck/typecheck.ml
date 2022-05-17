(* This file perform typechecking (a Brawn program. *)

open Brawn_ast.Ast

exception TypeCheckError of string

(* Create a hash set for global constants. *)
let constants = Hashtbl.create 10

(* Create a hash set for global variables. *)
let globals = Hashtbl.create 10

(* Create a hash set for user-defined functions. *)
let functions = Hashtbl.create 10

(* Whether a function is correctly called. *)
let verify_call f n = 
    if not (Hashtbl.mem functions f) then 
      raise (TypeCheckError "brawn: function does not exist.") else ();
    if (Hashtbl.find functions f) != n then
      raise (TypeCheckError "brawn: function called with incorrect number of arguments.") else ()

(* Whether or not to record an identifier in the globals *)
let record_global i ig = if List.mem i ig then ()
                         else Hashtbl.add globals i true

let rec visit_pattern = function
    | Begin 
    | End -> () 
    | Expr e -> visit_expr [] e 
    | Range (u, v) -> visit_expr [] u; visit_expr [] v

and visit_lvalue ig = function
    | IdentVal i -> record_global i ig
    | ArrayVal (i, es) -> record_global i ig;
                          ignore (List.map (visit_expr ig) es)
    | Dollar e -> (visit_expr ig) e

and visit_expr ig = function
    | BinaryOp (_, u, v) -> visit_expr ig u;
                            visit_expr ig v
    | UnaryOp (_, e) -> visit_expr ig e
    | FuncCall (f, es) ->  verify_call f (List.length es);
                          ignore (List.map (visit_expr ig) es)
    | Ternary (c, t, f) -> visit_expr ig c;
                           visit_expr ig t;
                           visit_expr ig f
    | Member (es, i) -> ignore (List.map (visit_expr ig) es);
                        record_global i ig
    | Getline None -> ()
    | Getline (Some l)    
    | Postfix (_, l)
    | Prefix (_, l)
    | LValue l -> visit_lvalue ig l
    | UpdateOp (_, l, e)
    | Assignment (l, e) -> visit_lvalue ig l; (visit_expr ig) e
    | Literal l -> Hashtbl.add constants l true

and visit_stmt ig = function
    | If (e, t, None) -> (visit_expr ig) e;
                         visit_stmt ig t
    | If (e, t, Some f) -> (visit_expr ig) e; 
                           visit_stmt ig t; 
                           visit_stmt ig f
    | While (e, s)
    | Do (s, e) -> visit_stmt ig s;
                   visit_expr ig e
    | For (is, e, us, s) -> ignore (Option.map (visit_stmt ig) is);
                            ignore (Option.map (visit_expr ig) e);
                            ignore (Option.map (visit_stmt ig) us);
                            visit_stmt ig s
    | RangedFor (_, _, s) -> visit_stmt ig s
    | Exit (Some e)
    | Return (Some e)
    | Expression e -> visit_expr ig e
    | Print es -> ignore (List.map (visit_expr ig) es)
    | Delete (i, es) -> record_global i ig;
                        ignore (List.map (visit_expr ig) es)
    | Block ss -> ignore (List.map (visit_stmt ig) ss)
    | Return (None)
    | Exit (None)
    | Break
    | Continue 
    | Next
    | Skip -> ()

let visit_func (Function (_, args, s)) = ignore (Option.map (visit_stmt args) s)

let record_func (Function (f, args, _)) = Hashtbl.add functions f (List.length args)

let visit_action (Action (p, s)) = ignore (Option.map visit_pattern p);
                                   ignore (Option.map (visit_stmt []) s)

(* Extract constants from a Brawn program. *)
let visit_program (Program (f, a)) = ignore (List.map record_func f);
                                     ignore (List.map visit_func f);
                                     ignore (List.map visit_action a)