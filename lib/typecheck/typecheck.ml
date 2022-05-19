(* This file performs typechecking of a Brawn program. *)

open Brawn_ast.Ast
open Builtins

exception TypeCheckError of string

(* Create a hash set for global constants. *)
let constants = Hashtbl.create 10

(* Create a hash set for global variables. *)
let globals = Hashtbl.create 10

(* Create a hash set for user-defined functions. *)
let functions = Hashtbl.create 30

(* Whether a function is correctly called. *)
let verify_call f n =
    if not (Hashtbl.mem functions f) then
      raise (TypeCheckError "brawn: function never defined.") else ();
    if (Hashtbl.find functions f) > n then
      raise (TypeCheckError "brawn: too many arguments in call.") else ()

(* Whether or not to record an identifier in the globals *)
let record_global ig i =
    if List.mem i ig then ()
    else Hashtbl.add globals i true

(* This large piece of code is just visiting each AST node
 * and collecting all literals and global variables, and
 * ensuring that function calls are correct, and that
 * return, break, and continue statements are never used
 * outside of their proper context.
 *)
let rec visit_pattern = function
    | Begin
    | End -> ()
    | Expr e -> visit_expr [] e
    | Range (u, v) -> visit_expr [] u; visit_expr [] v

and visit_lvalue ig = function
    | IdentVal i -> record_global ig i
    | ArrayVal (i, es) -> record_global ig i;
                          List.iter (visit_expr ig) es
    | Dollar e -> (visit_expr ig) e

and visit_expr ig = function
    | BinaryOp (_, u, v) -> visit_expr ig u;
                            visit_expr ig v
    | UnaryOp (_, e) -> visit_expr ig e
    | FuncCall (f, es) -> verify_call f (List.length es);
                          List.iter (visit_expr ig) es
    | Ternary (c, t, f) -> visit_expr ig c;
                           visit_expr ig t;
                           visit_expr ig f
    | Member (es, i) -> List.iter (visit_expr ig) es;
                        record_global ig i
    | Getline None -> ()
    | Getline (Some l)
    | Postfix (_, l)
    | Prefix (_, l)
    | LValue l -> visit_lvalue ig l
    | UpdateOp (_, l, e)
    | Assignment (l, e) -> visit_lvalue ig l; (visit_expr ig) e
    | Literal l -> Hashtbl.add constants l true

and visit_stmt ig loop func = function
    | If (e, t, f) -> visit_expr ig e;
                      visit_stmt ig loop func t;
                      Option.iter (visit_stmt ig loop func) f
    | While (e, s)
    | Do (s, e) -> visit_stmt ig true func s;
                   visit_expr ig e
    | For (is, e, us, s) -> Option.iter (visit_stmt ig false func) is;
                            Option.iter (visit_expr ig) e;
                            Option.iter (visit_stmt ig false func) us;
                            visit_stmt ig true func s
    | RangedFor (e, a, s) -> record_global ig e;
                             record_global ig a;
                             visit_stmt ig true func s
    | Exit (Some e)
    | Expression e -> visit_expr ig e
    | Return e -> if not func
                  then raise (TypeCheckError "brawn: return outside function body.")
                  else Option.iter (visit_expr ig) e
    | Print es -> List.iter (visit_expr ig) es
    | Delete (i, es) -> record_global ig i;
                        List.iter (visit_expr ig) es
    | Block ss -> List.iter (visit_stmt ig loop func) ss
    | Exit None
    | Next
    | Skip -> ()
    | Break -> if not loop then raise (TypeCheckError "brawn: break outside of loop.") else ()
    | Continue -> if not loop then raise (TypeCheckError "brawn: continue outside of loop.") else ()

let visit_func (Function (_, args, s)) = visit_stmt args false true s

let record_func (Function (f, args, _)) =
    if Hashtbl.mem functions f
    then raise (TypeCheckError "brawn: function cannot be redefined.")
    else Hashtbl.add functions f (List.length args)

let visit_action (Action (p, s)) =
    Option.iter visit_pattern p;
    Option.iter (visit_stmt [] false false) s

(* The entrypoint of the typechecking code. *)
let visit_program (Program (f, a)) =
    List.iter (fun (f, n) -> Hashtbl.add functions f n) builtin_functions;
    List.iter (fun l -> Hashtbl.add constants l true) builtin_consts;
    List.iter (record_global []) builtin_variables;
    List.iter record_func f;
    List.iter visit_func f;
    List.iter visit_action a
