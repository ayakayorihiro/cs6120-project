%{
     open Brawn_ast;;
     open Ast.Ast;;
%}

%token <string> NAME 
%token <float> NUMBER
%token <string> STRING
%token <string> ERE
%token <string> FUNC_NAME   /* Name followed by LPAREN without white space. */


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
%token BUILTIN_FUNC_NAME
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


%token OR   AND  NO_MATCH   EQ   LE   GE   NE   INCR  DECR  APPEND
/*     '||' '&&' '!˜' '==' '<=' '>=' '!=' '++'  '--'  '>>'   */

/* One-character tokens. */
%token LCURL RCURL LPAREN RPAREN LBRACK RBRACK COMMA SEMICOLON NEWLINE EOF
%token PLUS MINUS MULT MOD POW BANG GT LT PIPE QMARK COLON SQUIGGLE DOLLAR ASSIGN DIV

%nonassoc	DOLLAR
%nonassoc	INCR
%nonassoc	DECR
%right	POW
%nonassoc	BANG
%nonassoc	PLUS
%nonassoc	MINUS
%left	MULT
%left	DIV
%left	MOD
// %left	expr expr
%nonassoc	LT
%nonassoc	LE
%nonassoc	NE
%nonassoc	EQ
%nonassoc	GT
%nonassoc	GE
%nonassoc	SQUIGGLE
%nonassoc	NO_MATCH
%left	In
// %left	( index ) in array
	
%left	AND
%left	OR
%right	COLON
	
%right	POW_ASSIGN
%right	MOD_ASSIGN
%right	MULT_ASSIGN
%right	DIV_ASSIGN
%right	ADD_ASSIGN
%right	SUB_ASSIGN
%right	ASSIGN

%start <program> program
%%


program          : item_list { Program $1 }
                 | actionless_item_list { Program $1 }
                 ;


item_list        : newline_opt { [] }
                 | actionless_item_list item terminator { $1 @ [$2] }
                 | item_list            item terminator { $1 @ [$2] }
                 | item_list          action terminator { $1 @ [ActionDecl([], [$2])] }
                 ;


actionless_item_list
                 : item_list            pattern terminator { $1 @ [ActionDecl([$2], [])] }
                 | actionless_item_list pattern terminator { $1 @ [ActionDecl([$2], [])] }
                 ;


item             : pattern action { ActionDecl ([$1], [$2]) }
                 | Function NAME      LPAREN param_list_opt RPAREN
                       newline_opt action { FunctionDecl (Function (Identifier($2), $4, [$7])) }
                 | Function FUNC_NAME LPAREN param_list_opt RPAREN
                       newline_opt action { FunctionDecl (Function (Identifier($2), $4, [$7])) }
                 ;


param_list_opt   : /* empty */
                    { [] }
                 | param_list { $1 }
                 ;


param_list       : NAME { [Identifier($1)] }
                 | param_list COMMA NAME { $1 @ [Identifier($3)] }
                 ;


pattern          : Begin { Begin }
                 | End { End }
                 | expr_list { ExpressionList ($1) }
                 ;


action           : LCURL newline_opt                             RCURL { Block [] }
                 | LCURL newline_opt terminated_statement_list   RCURL { Block $3 }
               //   | LCURL newline_opt unterminated_statement_list RCURL { $3 }
                 ;


terminator       : terminator SEMICOLON { }
                 | terminator NEWLINE { }
                 |            SEMICOLON { }
                 |            NEWLINE { }
                 ;


terminated_statement_list
                 : terminated_statement { [$1] }
                 | terminated_statement_list terminated_statement { $1 @ [$2] }
                 ;


// unterminated_statement_list
//                  : unterminated_statement { }
//                  | terminated_statement_list unterminated_statement
//                  ;


terminated_statement : action newline_opt { $1 }
                 | If LPAREN expr RPAREN newline_opt terminated_statement { If ($3, $6, None) }
                 | If LPAREN expr RPAREN newline_opt terminated_statement
                       Else newline_opt terminated_statement  { If ($3, $6, Some($9))}
                 | While LPAREN expr RPAREN newline_opt terminated_statement { While ($3, $6) }
                 | For LPAREN simple_statement_opt SEMICOLON
                      expr_opt SEMICOLON simple_statement_opt RPAREN newline_opt
                      terminated_statement { For ($3, $5, $7, $10) }
                 | For LPAREN NAME In NAME RPAREN newline_opt
                      terminated_statement { RangedFor (Identifier($3), Identifier($5), $8) }
               //   | SEMICOLON newline_opt {  }
                 | terminatable_statement NEWLINE newline_opt { $1 }
                 | terminatable_statement SEMICOLON     newline_opt { $1 }
                 ;


// unterminated_statement : terminatable_statement
//                  | If LPAREN expr RPAREN newline_opt unterminated_statement
//                  | If LPAREN expr RPAREN newline_opt terminated_statement
//                       Else newline_opt unterminated_statement
//                  | While LPAREN expr RPAREN newline_opt unterminated_statement
//                  | For LPAREN simple_statement_opt SEMICOLON
//                   expr_opt SEMICOLON simple_statement_opt RPAREN newline_opt
//                       unterminated_statement
//                  | For LPAREN NAME In NAME RPAREN newline_opt
//                       unterminated_statement
//                  ;


terminatable_statement
                 : simple_statement { $1 }
                 | Break { Break }
                 | Continue { Continue }
                 | Next { Next }
                 | Exit expr_opt { Exit $2 }
                 | Return expr_opt { Return $2 }
                 | Do newline_opt terminated_statement While LPAREN expr RPAREN { Do ($3, $6) }
                 ;


simple_statement_opt : /* empty */
                    { None }
                 | simple_statement { Some ($1) }
                 ;


simple_statement : Delete NAME LBRACK expr_list RBRACK { Delete (Identifier($2), $4) }
                 | expr { Expression $1 }
                 | print_statement { Print $1 }
                 ;


print_statement  : simple_print_statement { $1 }
               //   | simple_print_statement output_redirection { }
                 ;


simple_print_statement
                 : Print  print_expr_list_opt { Print $2 }
                 | Print  LPAREN multiple_expr_list RPAREN { Print $3 }
                 | Printf print_expr_list { Printf $2 }
                 | Printf LPAREN multiple_expr_list RPAREN { Printf $3 }
                 ;

// FIXME: come back to this if we want output redirection
// output_redirection
//                  : GT    expr { }
//                  | APPEND expr
//                  | PIPE    expr
//                  ;


expr_list_opt    : /* empty */
                    { [] }
                 | expr_list { $1 }
                 ;


expr_list        : expr { [$1] }
                 | multiple_expr_list { $1 }
                 ;


multiple_expr_list
                 : expr COMMA newline_opt expr { $1 :: [$4] }
                 | multiple_expr_list COMMA newline_opt expr { $1 @ [$4] }
                 ;


expr_opt         : /* empty */
                    { None }
                 | expr { Some($1) }
                 ;


expr             : unary_expr { $1 }
               //   | non_unary_expr { $1 }
                 ;


unary_expr       : PLUS expr { Positive $2 }
                 | MINUS expr { Negative $2 }
                 | unary_expr POW      expr { Pow ($1, $3) }
                 | unary_expr MULT      expr { Multiply ($1, $3) }
                 | unary_expr DIV      expr { Divide ($1, $3) }
                 | unary_expr MOD      expr { Mod ($1, $3) }
                 | unary_expr PLUS      expr { Plus ($1, $3) }
                 | unary_expr MINUS      expr { Subtract ($1, $3) }
                 | unary_expr          unary_expr { Concat($1, $2) }
                 | unary_expr LT      expr { LessThan ($1, $3) }
                 | unary_expr LE       expr { LessThanEq ($1, $3) }
                 | unary_expr NE       expr { NotEquals ($1, $3) }
                 | unary_expr EQ       expr { Equals ($1, $3) }
                 | unary_expr GT      expr { GreaterThan ($1, $3) }
                 | unary_expr GE       expr { GreaterThanEq ($1, $3) }
                 | unary_expr SQUIGGLE      expr { Match ($1, $3) }
                 | unary_expr NO_MATCH expr { NonMatch ($1, $3) }
                 | unary_expr In NAME { Mem ($1, Identifier($3)) }
                 | unary_expr AND newline_opt expr { And($1, $4) }
                 | unary_expr OR  newline_opt expr { Or($1, $4) }
                 | unary_expr QMARK expr COLON expr { Ternary($1, $3, $5) }
               //   | unary_input_function { $1 }
                 // anything below here is from non_unary_expr lmao
                 | LPAREN expr RPAREN { $2 }
                 | BANG expr { Not $2 }
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
                 | FUNC_NAME LPAREN expr_list_opt RPAREN { FuncCall (Identifier($1), $3) }
                      /* no white space allowed before LPAREN */
               //   | BUILTIN_FUNC_NAME LPAREN expr_list_opt RPAREN
               //   | BUILTIN_FUNC_NAME
                 ;


// non_unary_expr   : LPAREN expr RPAREN
//                  | BANG expr
//                  | non_unary_expr POW      expr
//                  | non_unary_expr MULT      expr
//                  | non_unary_expr DIV      expr
//                  | non_unary_expr MOD      expr
//                  | non_unary_expr PLUS      expr
//                  | non_unary_expr MINUS      expr
//                  | non_unary_expr          non_unary_expr
//                  | non_unary_expr LT      expr
//                  | non_unary_expr LE       expr
//                  | non_unary_expr NE       expr
//                  | non_unary_expr EQ       expr
//                  | non_unary_expr GT      expr
//                  | non_unary_expr GE       expr
//                  | non_unary_expr SQUIGGLE      expr
//                  | non_unary_expr NO_MATCH expr
//                  | non_unary_expr In NAME
//                  | LPAREN multiple_expr_list RPAREN In NAME
//                  | non_unary_expr AND newline_opt expr
//                  | non_unary_expr OR  newline_opt expr
//                  | non_unary_expr QMARK expr COLON expr
//                  | NUMBER
//                  | STRING
//                  | lvalue
//                  | ERE
//                  | lvalue INCR
//                  | lvalue DECR
//                  | INCR lvalue
//                  | DECR lvalue
//                  | lvalue POW_ASSIGN expr
//                  | lvalue MOD_ASSIGN expr
//                  | lvalue MUL_ASSIGN expr
//                  | lvalue DIV_ASSIGN expr
//                  | lvalue ADD_ASSIGN expr
//                  | lvalue SUB_ASSIGN expr
//                  | lvalue ASSIGN expr
//                  | FUNC_NAME LPAREN expr_list_opt RPAREN
//                       /* no white space allowed before LPAREN */
//                  | BUILTIN_FUNC_NAME LPAREN expr_list_opt RPAREN
//                  | BUILTIN_FUNC_NAME
//                  | non_unary_input_function
//                  ;


print_expr_list_opt
                 : /* empty */
                 | print_expr_list { $1 }
                 ;


print_expr_list  : expr { [$1] }
                 | print_expr_list COMMA newline_opt expr { $1 @ [$4] }
                 ;


lvalue           : NAME { IdentVal (Identifier ($1)) }
                 | NAME LBRACK expr_list RBRACK { ArrayVal (Identifier($1), $3) }
                 | DOLLAR expr { Dollar ($2) }
                 ;


non_unary_input_function
                 : simple_get { $1 }
                 | simple_get LT expr { Redirect ($1, $3) }
                 | unary_expr PIPE simple_get { Pipe ($1, $3) }
                 ;


// unary_input_function
//                  : unary_expr PIPE simple_get { Pipe ($1, $3) }
//                  ;


simple_get       : GETLINE { Getline None }
                 | GETLINE lvalue { GetLine $2 }
                 ;


newline_opt      : /* empty */
                    { }
                 | newline_opt NEWLINE { }
                 ;
