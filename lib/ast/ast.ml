(* This file specifies the AST of a Brawn program. *)

(* Binary operations in Brawn *)
type binop =
    | Plus
    | Subtract
    | Multiply
    | Divide
    | Mod
    | Pow
    | LessThan
    | LessThanEq
    | Equals
    | NotEquals
    | GreaterThan
    | GreaterThanEq
    | Match
    | NonMatch
    | And
    | Or
    | Concat
[@@deriving show]

(* Unary operations in Brawn *)
type unop =
    | Negative
    | Positive
    | Not
[@@deriving show]

(* In-place update operations in Brawn *)
type updateop =
    | PowAssign
    | ModAssign
    | MulAssign
    | DivAssign
    | AddAssign
    | SubAssign
[@@deriving show]

(* Prefix and postfix operations in Brawn *)
type prepostops =
    | Incr
    | Decr
[@@deriving show]

(* Identifiers for variable names and functions *)
type ident = Identifier of string
[@@deriving show]

(* Literals in Brawn *)
type literal =
    | Number of float
    | String of string
    | Regexp of string
[@@deriving show]

(* Pattern to match in the patter-action pairs *)
type pattern = Begin | End | Expr of expr
[@@deriving show]

(* Lvalues are values that can be referenced by name *)
and lvalue =
    | IdentVal of ident
    | ArrayVal of ident * expr list
    | Dollar of expr
[@@deriving show]

(* Expressions in Brawn *)
and expr =
    | BinaryOp of binop * expr * expr
    | UnaryOp of unop * expr
    | UpdateOp of updateop * lvalue * expr
    | Getline of lvalue option
    | FuncCall of ident * expr list
    | Ternary of expr * expr * expr
    | Member of expr list * ident
    | Postfix of prepostops * lvalue
    | Prefix of prepostops * lvalue
    | LValue of lvalue
    | Assignment of lvalue * expr
    | Literal of literal
[@@deriving show]

(* Statements in Brawn *)
type statement =
    | If of expr * statement * statement option
    | While of expr * statement
    | Do of statement * expr
    | For of statement option * expr option * statement option * statement
    | RangedFor of ident * ident * statement
    | Break
    | Continue
    | Next
    | Exit of expr option
    | Return of expr option
    | Delete of ident * expr list
    | Print of expr list
    | Expression of expr
    | Block of statement list
    | Skip
[@@deriving show]

(* User-defined functions in Brawn *)
type func = Function of ident * ident list * statement
[@@deriving show]

(* Pattern-action pair *)
type action = Action of pattern option * statement option
[@@deriving show]

(* Brawn program *)
type program = Program of func list * action list
[@@deriving show]
