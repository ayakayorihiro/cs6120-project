(* This fine specifies the AST of a Brawn program. *)

(* Pattern to match in the patter-action pairs *)
type pattern = Begin | End | Expr of expr | Range of expr * expr
[@@deriving show]

(* Identifiers for variable names and functions *)
and ident = Identifier of string
[@@deriving show]

(* Lvalues are values that can be referenced by name *)
and lvalue =
    | IdentVal of ident
    | ArrayVal of ident * expr list
    | Dollar of expr
[@@deriving show]

(* Expressions in Brawn *)
and expr =
    | Getline of lvalue option
    | FuncCall of ident * expr list
    | Plus of expr * expr
    | Subtract of expr * expr
    | Multiply of expr * expr
    | Divide of expr * expr
    | Mod of expr * expr
    | Pow of expr * expr
    | LessThan of expr * expr
    | LessThanEq of expr * expr
    | Equals of expr * expr
    | NotEquals of expr * expr
    | GreaterThan of expr * expr
    | GreaterThanEq of expr * expr
    | Match of expr * expr
    | NonMatch of expr * expr
    | Regexp of string
    | And of expr * expr
    | Or of expr * expr
    | Ternary of expr * expr * expr
    | Member of expr * ident
    | Negative of expr
    | Positive of expr
    | Not of expr
    | LIncr of lvalue
    | LDecr of lvalue
    | RIncr of lvalue
    | RDecr of lvalue
    | LValue of lvalue
    | Number of float
    | String of string
    | Concat of expr * expr
    | PowAssign of lvalue * expr
    | ModAssign of lvalue * expr
    | MulAssign of lvalue * expr
    | DivAssign of lvalue * expr
    | AddAssign of lvalue * expr
    | SubAssign of lvalue * expr
    | Assignment of lvalue * expr
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
type func = Function of ident * ident list * statement option
[@@deriving show]

(* Pattern-action pair *)
type action = Action of pattern option * statement option
[@@deriving show]

(* Brawn program *)
type program = Program of func list * action list
[@@deriving show]
