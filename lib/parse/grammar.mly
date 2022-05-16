%{
     open Brawn_ast.Ast;;
     let debug_print content = 
     let _ = Printf.printf content in ()
     ;;
%}

%token <string> NAME 
%token <float> NUMBER
%token <string> STRING
%token <string> ERE
%token <string> FUNC_NAME   /* Name followed by LPAREN without white space. */
%token EOF

/* Keywords  */
%token       Begin   End
/*          'BEGIN' 'END'                            */


%token       Break   Continue   Delete   Do   Else
/*          'break' 'continue' 'delete' 'do' 'else'  */


%token       Exit   For   Function   If   In
/*          'exit' 'for' 'function' 'if' 'in'        */


%token       Next   Print   Printf   Return   While
/*          'next' 'print' 'printf' 'return' 'while' */


/* Reserved function names */
%token <string> BUILTIN_FUNC_NAME
            /* One token for the following:
             * atan2 cos sin exp log sqrt int rand srand
             * gsub index length match split sprintf sub
             * substr tolower toupper close system
             */
%token GETLINE
            /* Syntactically different from other built-ins. */


/* Two-character tokens. */
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN POW_ASSIGN
/*     '+='       '-='       '*='       '/='       '%='       '^=' */


%token OR   AND  NO_MATCH   EQ   LE   GE   NE   INCR  DECR  // APPEND
/*    '||' '&&'  '!˜'      '==' '<=' '>=' '!=' '++'  '--'  '>>'   */
// Add APPEND back if we dedide we want output redirection

/* One-character tokens. */
%token LCURL RCURL LPAREN RPAREN LBRACK RBRACK COMMA SEMICOLON // EOF
%token ForLPAREN WhileLPAREN IfLPAREN
%token PLUS MINUS MULT MOD POW BANG GT LT PIPE QMARK COLON SQUIGGLE DOLLAR ASSIGN DIV

/* lowest precedence - always reduce */
%right	ASSIGN
%right	SUB_ASSIGN ADD_ASSIGN
%right	DIV_ASSIGN MUL_ASSIGN MOD_ASSIGN
%right	POW_ASSIGN
%right	COLON
%left	OR
%left	AND
// %left	( index ) in array
%left	In
%nonassoc	NO_MATCH SQUIGGLE
%nonassoc	GE GT EQ NE LE LT
// %left	expr expr
%left	MOD DIV MULT
%nonassoc	PLUS MINUS
%nonassoc	BANG
%right	POW
%nonassoc	INCR DECR
%nonassoc	DOLLAR
 /* highest precedence - always shift */

%start <program> program
%%


program          : item_list EOF { debug_print "PProgram: item_list EOF\n%!" ; Program $1 }
                 | actionless_item_list EOF { debug_print "PProgram: actionless_item_list EOF\n%!" ; Program $1 }
                 | EOF { Program [] }
                 ;


/* NOTE: actual AWK does not necessitate a semicolon at the end of each item, but because the parser was being
   finnicky with newlines and the such, I defered to forcing each item needing to be ended with a semicolon.
*/
item_list        : actionless_item_list item SEMICOLON { debug_print "PItem_list: actionless_item_list item" ; $1 @ [$2] }
                 | item_list            item SEMICOLON { debug_print "PItem_list: item_list item" ; $1 @ [$2] }
                 | item_list          action SEMICOLON { debug_print "PItem_list: item_list action" ; $1 @ [ActionDecl([], [$2])] }
                 | { debug_print "PItem_list: Empty item\n" ; [] }
                 ;


actionless_item_list
                 : item_list            pattern SEMICOLON { $1 @ [ActionDecl([$2], [])] }
                 | actionless_item_list pattern SEMICOLON { $1 @ [ActionDecl([$2], [])] }
                 ;


item             : pattern action { debug_print "PItem: pattern action\n"; ActionDecl ([$1], [$2]) }
                 | Function NAME      LPAREN param_list_opt RPAREN action 
                    { FunctionDecl (Function (Identifier($2), $4, [$6])) }
                 | Function FUNC_NAME LPAREN param_list_opt RPAREN action 
                    { FunctionDecl (Function (Identifier($2), $4, [$6])) }
                 ;


param_list_opt   : { [] } /* empty */
                 | param_list { $1 }
                 ;


param_list       : NAME { [Identifier($1)] }
                 | param_list COMMA NAME { $1 @ [Identifier($3)] }
                 ;


pattern          : Begin { debug_print "beginnnnnnn\n%!"; Begin }
                 | End { End }
                 | expr_list { ExpressionList ($1) }
                 ;


action           : LCURL RCURL { debug_print "PAction: LCURL RCURL\n%!" ; Block [] }
                 | LCURL terminated_statement_list   RCURL { Printf.printf "PAction: LCURL terminated_statement_list   RCURL\n%!" ; Block $2 }
               //   | LCURL unterminated_statement_list RCURL { Printf.printf "PAction: LCURL unterminated_statement_list bRCURL\n%!" ; $2 }
                 ;
// BRAWN: 
// We are not going to allow unterminated statements. 
// Instead, we will require that parens be used to create terminated statements


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
                 | IfLPAREN expr RPAREN  terminated_statement { If ($2, $4, None) }
                 | IfLPAREN expr RPAREN  terminated_statement
                       Else  terminated_statement  { If ($2, $4, Some($6))}
                 | WhileLPAREN expr RPAREN  terminated_statement { While ($2, $4) }
                 | ForLPAREN simple_statement_opt SEMICOLON
                      expr_opt SEMICOLON simple_statement_opt RPAREN 
                      terminated_statement 
                      { debug_print "PTerminated_statement: For (simple_statement_opt; expr_opt; simple_statement_opt) terminated_statement";
                      For ($2, $4, $6, $8) }
                 | ForLPAREN NAME In NAME RPAREN
                      terminated_statement { RangedFor (Identifier($2), Identifier($4), $6) }
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


// BRAWN: 
// We are not going to allow unterminated statements. 
// Instead, we will require that parens be used to create terminated statements

// unterminated_statement_list
//                  : unterminated_statement { }
//                  | terminated_statement_list unterminated_statement
//                  ;

// unterminated_statement : terminatable_statement
//                  | If LPAREN expr RPAREN  unterminated_statement
//                  | If LPAREN expr RPAREN  terminated_statement
//                       Else  unterminated_statement
//                  | While LPAREN expr RPAREN  unterminated_statement
//                  | For LPAREN simple_statement_opt SEMICOLON
//                   expr_opt SEMICOLON simple_statement_opt RPAREN 
//                       unterminated_statement
//                  | For LPAREN NAME In NAME RPAREN 
//                       unterminated_statement
//                  ;


terminatable_statement
                 : simple_statement { $1 }
                 | Break { Break }
                 | Continue { Continue }
                 | Next { Next }
                 | Exit expr_opt { Exit $2 }
                 | Return expr_opt { Return $2 }
                 | Do  terminated_statement While LPAREN expr RPAREN { Do ($2, $5) }
                 ;


simple_statement_opt : { None } /* empty */
                     | simple_statement { Some ($1) }
                     ;


simple_statement : Delete NAME LBRACK expr_list RBRACK { Delete (Identifier($2), $4) }
                 | expr { Expression $1 }
                 | print_statement { Print $1 }
                 ;


print_statement  : simple_print_statement { $1 }
               //   | simple_print_statement output_redirection { }
                 ;;
// BRAWN FIXME: come back to this if we want output redirection

// BRAWN FIXME: come back to this if we want output redirection
// output_redirection
//                  : GT    expr { }
//                  | APPEND expr
//                  | PIPE    expr
//                  ;

simple_print_statement
                 : Print  print_expr_list_opt { Print $2 }
                 | Print  LPAREN multiple_expr_list RPAREN { Print $3 }
                 | Printf print_expr_list { Printf $2 }
                 | Printf LPAREN multiple_expr_list RPAREN { Printf $3 }
                 ;

expr_list_opt    : /* empty */
                    { [] }
                 | expr_list { $1 }
                 ;


expr_list        : expr { [$1] }
                 | multiple_expr_list { $1 }
                 ;


multiple_expr_list
                 : expr COMMA  expr { $1 :: [$3] }
                 | multiple_expr_list COMMA  expr { $1 @ [$3] }
                 ;


expr_opt         : { None } /* empty */
                 | expr { Some($1) }
                 ;


expr             : unary_expr { $1 }
                 | non_unary_expr { $1 }
                 ;

unary_expr       : PLUS expr { Positive $2 }
                 | MINUS expr { Negative $2 }
                 | unary_expr POW expr { Pow ($1, $3) }
                 | unary_expr MULT expr { Multiply ($1, $3) }
                 | unary_expr DIV expr { Divide ($1, $3) }
                 | unary_expr MOD expr { Mod ($1, $3) }
                 | unary_expr PLUS expr { Plus ($1, $3) }
                 | unary_expr MINUS expr { Subtract ($1, $3) }
                 | unary_expr non_unary_expr { Concat($1, $2) }
                 | unary_expr LT expr { LessThan ($1, $3) }
                 | unary_expr LE expr { LessThanEq ($1, $3) }
                 | unary_expr NE expr { NotEquals ($1, $3) }
                 | unary_expr EQ expr { Equals ($1, $3) }
                 | unary_expr GT expr { GreaterThan ($1, $3) }
                 | unary_expr GE expr { GreaterThanEq ($1, $3) }
                 | unary_expr SQUIGGLE expr { Match ($1, $3) }
                 | unary_expr NO_MATCH expr { NonMatch ($1, $3) }
                 | unary_expr In NAME { Mem ($1, Identifier($3)) }
                 | unary_expr AND  expr { And ($1, $3) }
                 | unary_expr OR   expr { Or ($1, $3) }
                 | unary_expr QMARK expr COLON expr { Ternary ($1, $3, $5) }
                //  | non_unary_input_function { Input $1 }
                // BRAWN: this particular case is handled along with non-unary
                 ;

non_unary_expr   : LPAREN expr RPAREN { $2 }
                 | BANG expr { Not $2 }
                 | non_unary_expr POW expr { Pow ($1, $3) }
                 | non_unary_expr MULT expr { Multiply ($1, $3) }
                 | non_unary_expr DIV expr { Divide ($1, $3) }
                 | non_unary_expr MOD expr { Mod ($1, $3) }
                 | non_unary_expr PLUS expr { Plus ($1, $3) }
                 | non_unary_expr MINUS expr { Subtract ($1, $3) }
                 | non_unary_expr non_unary_expr { Concat ($1, $2) }
                 | non_unary_expr LT expr { LessThan ($1, $3) }
                 | non_unary_expr LE expr { LessThanEq ($1, $3) }
                 | non_unary_expr NE expr { NotEquals ($1, $3) }
                 | non_unary_expr EQ expr { Equals ($1, $3) }
                 | non_unary_expr GT expr { GreaterThan ($1, $3) }
                 | non_unary_expr GE expr { GreaterThanEq ($1, $3) }
                 | non_unary_expr SQUIGGLE expr { Match ($1, $3) }
                 | non_unary_expr NO_MATCH expr { NonMatch ($1, $3) }
                 | non_unary_expr In NAME { Mem ($1, Identifier($3)) }
                //  | LPAREN multiple_expr_list RPAREN In NAME
                // BRAWN: this is for multi-dimensional array membership.
                // punting for now; let's talk about whether we want to 
                // support this all around.
                 | non_unary_expr AND  expr { And ($1, $3) }
                 | non_unary_expr OR   expr { Or ($1, $3) }
                 | non_unary_expr QMARK expr COLON expr { Ternary ($1, $3, $5) }
                 | NUMBER { Number $1 }
                 | STRING { String $1 }
                 | lvalue { LValue $1 }
                 | ERE { String $1 }
                 | lvalue INCR { LIncr $1 }
                 | lvalue DECR { LDecr $1 } 
                 | INCR lvalue { RIncr $2 }
                 | DECR lvalue { RDecr $2 }
                 | lvalue POW_ASSIGN expr { PowAssign ($1, $3) }
                 | lvalue MOD_ASSIGN expr { ModAssign ($1, $3) }
                 | lvalue MUL_ASSIGN expr { MulAssign ($1, $3) }
                 | lvalue DIV_ASSIGN expr { DivAssign ($1, $3) }
                 | lvalue ADD_ASSIGN expr { AddAssign ($1, $3) }
                 | lvalue SUB_ASSIGN expr { SubAssign ($1, $3) }
                 | lvalue ASSIGN expr { Assignment ($1, $3) }
                 | FUNC_NAME LPAREN expr_list_opt RPAREN { FuncCall (Identifier $1, $3) }
                      /* no white space allowed before LPAREN */
                 | BUILTIN_FUNC_NAME LPAREN expr_list_opt RPAREN { FuncCall (Identifier $1, $3) }
                 | BUILTIN_FUNC_NAME { FuncCall (Identifier $1, []) }
                 // BRAWN: maybe these should be separate from regular func calls?
                 | non_unary_input_function { Input $1 }
                 ;


print_expr_list_opt : /* empty */
                    | print_expr_list { $1 }
                    ;


print_expr_list  : expr { [$1] }
                 | print_expr_list COMMA  expr { $1 @ [$3] }
                 ;

// BRAWN: We have nixed unary_print_expr and non_unary_print_expr

lvalue           : NAME { IdentVal (Identifier ($1)) }
                 | NAME LBRACK expr_list RBRACK { ArrayVal (Identifier($1), $3) }
                 | DOLLAR expr { Dollar ($2) }
                 ;

non_unary_input_function
                 : simple_get { SimpleGet $1 }
                 | simple_get LT expr { Redirect ($1, $3) }
                 | unary_expr PIPE simple_get { Pipe ($1, $3) }

// BRAWN: there is no need for this, because the third constructor above
// can handle unary and non-unary cases. 
// unary_input_function
                //  : unary_expr PIPE simple_get { Pipe ($1, $3) }
                //  ;


simple_get       : GETLINE { Getline None }
                 | GETLINE lvalue { Getline (Some $2) }
                 ;