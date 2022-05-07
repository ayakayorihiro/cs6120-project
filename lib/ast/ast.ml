module Ast = struct

type pattern = Begin | End | ExpressionList of expr list

and ident = Identifier of string

and input =
       SimpleGet of simple_get
     | Pipe of expr * simple_get
     | Redirect of simple_get * expr

and simple_get =  Getline of lvalue option

and lvalue =
      IdentVal of ident
    | ArrayVal of ident * expr list
    | Dollar of expr

and expr =
      Input of input
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
    | Mem of expr * ident
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

type print =
      Print of expr list
    | Printf of expr list

type statement =
      If of expr * statement * statement option
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
    | Print of print
    | Expression of expr
    | Block of statement list

type func = Function of ident * ident list * statement list

type item =
      FunctionDecl of func
    | ActionDecl of pattern list * statement list

type program = Program of item list

end