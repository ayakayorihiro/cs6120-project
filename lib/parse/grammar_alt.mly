%{
     open Brawn_ast;;
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


/* Reserved function names */
%token <string> BUILTIN_FUNC_NAME
            /* One token for the following:
             * atan2 cos sin exp log sqrt int rand srand
             * gsub index length match split sprintf sub
             * substr tolower toupper close system
             */
/* %token GETLINE */
            /* Syntactically different from other built-ins. */


%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN POW_ASSIGN
%token OR   AND  NO_MATCH   EQ   LE   GE   NE   INCR  DECR  // APPEND
%token LCURL RCURL LPAREN RPAREN LBRACK RBRACK COMMA SEMICOLON // EOF
%token PLUS MINUS MULT MOD POW BANG GT LT QMARK COLON SQUIGGLE DOLLAR ASSIGN DIV //  PIPE

/** Define precedences of operators. */
%right    ASSIGN
%right    SUB_ASSIGN ADD_ASSIGN
%right    DIV_ASSIGN MUL_ASSIGN MOD_ASSIGN
%right    POW_ASSIGN
%right    COLON
%left     OR
%left     AND
%left     In
%nonassoc NO_MATCH SQUIGGLE
%nonassoc GE GT EQ NE LE LT
%left     MOD DIV MULT
%nonassoc PLUS MINUS
%nonassoc BANG
%right    POW
%nonassoc INCR DECR
%nonassoc DOLLAR

%start <program> program
%%


program          : function_item_list item_list EOF { debug_print "PProgram: item_list EOF\n%!" ; Program ($1, $2) }
                 | function_item_list actionless_item_list EOF { debug_print "PProgram: actionless_item_list EOF\n%!" ; Program($1, $2) }
                 | EOF { Program([], []) }
                 ;


/* NOTE: actual AWK does not necessitate a semicolon at the end of each item, but because the parser was being
   finnicky with newlines and the such, I defered to forcing each item needing to be ended with a semicolon.
*/
item_list        : actionless_item_list item SEMICOLON { 
                         debug_print "PItem_list: actionless_item_list item" ;
                         $1 @ [$2] 
                         }
                 | item_list item SEMICOLON { 
                      debug_print "PItem_list: item_list item\n" ; $1 @ [$2] }
                 | item_list action SEMICOLON { debug_print "PItem_list: item_list action\n" ; $1 @ [Action(None, Some $2)] }
                 /* FIXME: Note that we may just want to make a separate constructor in pattern that is just... "None" */
                 | { debug_print "PItem_list: Empty item\n" ; [] }
                 ;


actionless_item_list
                 : item_list            pattern SEMICOLON { $1 @ [Action(Some $2, None)] }
                 | actionless_item_list pattern SEMICOLON { $1 @ [Action(Some $2, None)] }
                 ;


item             : pattern action { debug_print "PItem: pattern action\n"; Action (Some $1, Some $2) }

function_item_list: 
     function_item_list function_item { $1 @ [$2] }
     | function_item { [$1] } 
     | {[]}

function_item:
                 Function NAME      LPAREN param_list_opt RPAREN action 
                    { Function (Identifier($2), $4, Some $6) }
                 | Function FUNC_NAME LPAREN param_list_opt RPAREN action 
                    { Function (Identifier($2), $4, Some $6) }
                 ;


param_list_opt   : { [] } /* empty */
                 | param_list { $1 }
                 ;


param_list       : NAME { [Identifier($1)] }
                 | param_list COMMA NAME { $1 @ [Identifier($3)] }
                 ;


pattern          : Begin { debug_print "beginnnnnnn\n%!"; Begin }
                 | End { End }
                 | expr { Expr ($1) }
                 | expr COMMA expr { Range ($1, $3) }
                 ;


action           : LCURL RCURL { debug_print "PAction: LCURL RCURL\n%!" ; Block [] }
                 | LCURL terminated_statement_list   RCURL { debug_print "PAction: LCURL terminated_statement_list   RCURL\n%!" ; Block $2 }
               //   | LCURL unterminated_statement_list RCURL { debug_print "PAction: LCURL unterminated_statement_list bRCURL\n%!" ; $2 }
                 ;

terminated_statement_list
                 : terminated_statement { [$1] }
                 | terminated_statement_list terminated_statement { $1 @ [$2] }
                 ;

terminated_statement : action  { $1 }
                 | If LPAREN expr RPAREN  terminated_statement { If ($3, $5, None) }
                 | If LPAREN expr RPAREN  terminated_statement
                       Else  terminated_statement  { If ($3, $5, Some($7))}
                 | While LPAREN expr RPAREN  terminated_statement { While ($3, $5) }
                 | For LPAREN simple_statement_opt SEMICOLON
                      expr_opt SEMICOLON simple_statement_opt RPAREN 
                      terminated_statement 
                      { debug_print "PTerminated_statement: For (simple_statement_opt; expr_opt; simple_statement_opt) terminated_statement";
                      For ($3, $5, $7, $9) }
                 | For LPAREN NAME In NAME RPAREN
                      terminated_statement { RangedFor (Identifier($3), Identifier($5), $7) }
                      /* NOTE: the below is a hack to prevent if/for/while without spaces 
                      before the parens to be accidentally interpreted as function names */
                 | SEMICOLON  { Skip }
               // BRAWN: Hmm, this used to be 
               //  | SEMICOLON  { }
               // but that seemed like a strange thing to punt on.
               // so I added "Skip" as a constructor to the AST and used that here.
               // if 
               // a) leaving the line commented out over here 
               // is the same as
               // b) having the line here and Skip-ing explicitly, 
               // that's fine...       
                 | terminatable_statement { $1 }
                 | terminatable_statement SEMICOLON      { $1 }
                 ;

terminatable_statement
                 : simple_statement { $1 }
                 | Break { Break }
                 | Continue { Continue }
                 | Next { Next }
                 | Exit expr_opt { Exit $2 }
                 | Return expr_opt { Return $2 }
                 | Do terminated_statement While LPAREN expr RPAREN { Do ($2, $5) }
                 ;

simple_statement_opt :
                 option(simple_statement)

simple_statement : Delete NAME LBRACK expr_list RBRACK { Delete (Identifier($2), $4) }
                 | expr { Expression $1 }
                 | print_statement { $1 }
                 ;


simple_print_statement
                 : Print  expr_list_opt { Print $2 }

expr_list_opt: seperated_list(COMMA, expr)

expr_list: seperated_nonempty_list(COMMA, expr)

expr_opt: option(expr)

lvalue:
    | NAME { IdentVal (Identifier ($1)) }
    | NAME LBRACK expr_list RBRACK { ArrayVal (Identifier($1), $3) }
    | DOLLAR expr { Dollar ($2) }

expr:
    | LPAREN expr RPAREN { $2 }
    | o=un_op e=expr { UnaryOp (o, e) }
    | u=expr o=bin_op v=expr { BinaryOp (o, u, v) }
    | l=lvalue o=update_op e=expr { UpdateOp (o, l, e) }
    | e=expr o=incr_op { Postfix (o, e) }
    | o=incr_op e=expr { Prefix (o, e) }
    | p=pair(expr, expr) { Concat p }
    | e=expr In id=NAME { Mem ($1, Identifier($3)) }
    | LPAREN expr_list RPAREN In NAME
    | expr QMARK expr COLON expr { Ternary ($1, $3, $5) }
    | n=NUMBER { Number n }
    | s=STRING { String s }
    | l=lvalue { LValue l }
    | r=ERE { Regexp e }
    | f=NAME LPAREN args=expr_list_opt RPAREN { FuncCall (f, args) }
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
