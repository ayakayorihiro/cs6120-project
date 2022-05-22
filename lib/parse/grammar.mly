%{
    open Brawn_ast.Ast
%}

/* Keywords  */
%token       Begin   End
/*          'BEGIN' 'END'                            */

%token       Break   Continue   Delete   Do   Else
/*          'break' 'continue' 'delete' 'do' 'else'  */

%token       Exit   For   Function   If   In
/*          'exit' 'for' 'function' 'if' 'in'        */

%token       Next   Print   Return   While
/*          'next' 'print' 'return' 'while' */

/* Syntactically different from other built-ins. */
%token       GETLINE
/*          'getline' */

/* Tokens for binary and unary operators. */
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN POW_ASSIGN
%token OR   AND  NO_MATCH   EQ   LE   GE   NE   INCR  DECR
%token PLUS MINUS MULT MOD POW BANG GT LT QMARK COLON SQUIGGLE DOLLAR ASSIGN DIV CONCAT

/* Tokens for punctuation. */
%token LCURL RCURL LPAREN RPAREN LBRACK RBRACK COMMA SEMICOLON EOF

/* Basic tokens for literals and identifiers. */
%token <string> STRING
%token <string> ERE
%token <float>  NUMBER
%token <string> NAME

/** Define precedences of operators. */
%right    ASSIGN
%right    SUB_ASSIGN ADD_ASSIGN
%right    DIV_ASSIGN MUL_ASSIGN MOD_ASSIGN
%right    POW_ASSIGN
%right    COLON QMARK
%left     OR
%left     AND
%left     In
%nonassoc NO_MATCH SQUIGGLE
%nonassoc GE GT EQ NE LE LT
%left     CONCAT
%nonassoc PLUS MINUS
%left     MOD DIV MULT
%nonassoc BANG
%right    POW
%nonassoc INCR DECR
%nonassoc DOLLAR

/* The start production. */
%start program

/* Specifying the types of productions. */
%type <statement> statement
%type <literal>   literal
%type <updateop>  update_op
%type <program>   program
%type <binop>     bin_op
%type <unop>      un_op
%type <expr>      expr

%%

program: fs=list(function_item) aa=list(item) EOF { Program (fs, aa) }

item:
     | a=statement { Action (None, Some a) }
     | p=pattern { Action (Some p, None) }
     | p=pattern a=statement { Action (Some p, Some a) }

function_item: Function n=identifier LPAREN args=param_list_opt RPAREN s=statement { Function (n, args, s) }

param_list_opt: ls=separated_list(COMMA, identifier) { ls }

pattern:
     | Begin { Begin }
     | End { End }
     | e=expr { Expr e }

statement_list: ls=nonempty_list(statement) { ls };

statement:
     | If LPAREN e=expr RPAREN s=statement { If (e, s, None) }
     | If LPAREN e=expr RPAREN ts=statement Else fs=statement { If (e, ts, Some(fs))}
     | While LPAREN e=expr RPAREN s=statement { While (e, s) }
     | For LPAREN is=simple_statement_opt SEMICOLON e=expr_opt SEMICOLON us=simple_statement_opt RPAREN s=statement { For (is, e, us, s) }
     | For LPAREN a=identifier In arr=identifier RPAREN s=statement { RangedFor (a, arr, s) }
     | SEMICOLON  { Skip }
     | s=simple_statement { s }
     | Break { Break }
     | Continue { Continue }
     | Next { Next }
     | Exit e=expr_opt { Exit e }
     | Return e=expr_opt { Return e }
     | Do s=statement While LPAREN e=expr RPAREN { Do (s, e) }
     | LCURL ss=statement_list RCURL { Block ss }

simple_statement_opt :
     | s=option(simple_statement) { s }

simple_statement:
     | Delete n=identifier LBRACK es=expr_list RBRACK { Delete (n, es) }
     | e=expr { Expression e }
     | Print es=expr_list_opt { Print es }

expr_list_opt: es=separated_list(COMMA, expr) { es } ;

expr_list: es=separated_nonempty_list(COMMA, expr) { es }

expr_opt: e=option(expr) { e }

lvalue:
    | n=identifier { IdentVal n }
    | n=identifier LBRACK es=expr_list RBRACK { ArrayVal (n, es) }
    | DOLLAR e=expr { Dollar e }

literal:
    | n=NUMBER { Number n }
    | s=STRING { String s }
    | r=ERE { Regexp r }

expr:
    | LPAREN e=expr RPAREN { e }
    | o=un_op e=expr { UnaryOp (o, e) }
    | u=expr o=bin_op v=expr { BinaryOp (o, u, v) }
    | l=lvalue o=update_op e=expr { UpdateOp (o, l, e) }
    | l=lvalue o=incr_op { Postfix (o, l) }
    | o=incr_op l=lvalue { Prefix (o, l) }
    | LBRACK es=expr_list RBRACK In n=identifier { Member (es, n) }
    | c=expr QMARK tv=expr COLON fv=expr { Ternary (c, tv, fv) }
    | l=lvalue ASSIGN e=expr { Assignment (l, e) }
    | c=literal { Literal c }
    | l=lvalue { LValue l }
    | f=identifier LPAREN args=expr_list_opt RPAREN { FuncCall (f, args) }
    | GETLINE l=option(lvalue) { Getline l }

%inline update_op:
    | POW_ASSIGN { PowAssign }
    | MOD_ASSIGN { ModAssign }
    | MUL_ASSIGN { MulAssign }
    | DIV_ASSIGN { DivAssign }
    | ADD_ASSIGN { AddAssign }
    | SUB_ASSIGN { SubAssign }

%inline incr_op:
    | INCR { Incr }
    | DECR { Decr }

%inline un_op:
    | BANG  { Negative }
    | MINUS { Negative }
    | PLUS  { Positive }

%inline bin_op:
    | PLUS     { Plus }
    | MINUS    { Subtract }
    | MULT     { Multiply }
    | DIV      { Divide }
    | MOD      { Mod }
    | LT       { LessThan }
    | LE       { LessThanEq }
    | GT       { GreaterThan }
    | GE       { GreaterThanEq }
    | AND      { And }
    | OR       { Or }
    | EQ       { Equals }
    | NE       { NotEquals }
    | SQUIGGLE { Match }
    | NO_MATCH { NonMatch }
    | POW      { Pow }
    | CONCAT   { Concat }

identifier: n=NAME { Identifier n }
