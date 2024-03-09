%{
    open ImitatorProperty
%}

%token <string> NUM
%token <string> NAME
%token <string> BINARYWORD
%token <string> STRING
%token <ImitatorProperty.id> ID
%token <string> EMBEDDED
%token <string> COUNTVar
%token <string> LASTVar

%token OP_PLUS OP_MINUS OP_MUL OP_DIV
%token OP_L OP_LEQ OP_EQ OP_NEQ OP_GEQ OP_G OP_ASSIGN

%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
%token COLON COMMA DOUBLEDOT OP_CONJUNCTION OP_DISJUNCTION OP_IMPLIES SEMICOLON

%token LET IN VAR MOD USE GLOBAL_TIME

%token EOF

%left OP_DISJUNCTION OP_IMPLIES /* lowest precedence */
%left OP_CONJUNCTION            /* medium precedence */
%left DOUBLEDOT                 /* high precedence */
%nonassoc CT_NOT                /* highest precedence */

%left OP_PLUS OP_MINUS          /* lowest precedence */
%left OP_MUL OP_DIV             /* highest precedence */


%start main             /* the entry point */
%type <ImitatorProperty.t> main
%%

main:
  | raw_spec=EMBEDDED EOF {
                        let begin_pattern = Str.regexp "<\*[ \t\n]+" in
                        let end_pattern = Str.regexp "[ \t\n]+\*>" in
                        let spec = Str.replace_first begin_pattern "" raw_spec
                                   |> Str.replace_first end_pattern "" in
                        Property (false, [], [], spec) }
  | USE GLOBAL_TIME SEMICOLON p=main {
                                    match p with
                                    | Property (_, m, v, r) -> Property (true, m, v, r)}
  | LET id=ID OP_EQ expr=boolean_expression IN p=main {
                                                     match p with
                                                     | Property (b, m, v, r) -> Property (b, m, ((Valuation (id, expr)) :: v), r)}
  | VAR var=COUNTVar COLON MOD modulo=NUM p=main {
                                                match p with
                                                | Property (b, m, v, r) ->
                                                   try
                                                     Property (b, ((CountMod (var, int_of_string modulo)) :: m), v, r)
                                                   with Failure _ -> raise (Failure ("Uninteger value is passed as modulo: " ^ modulo))}
;

/************************************************************/
/** BOOLEAN EXPRESSIONS */
/************************************************************/

boolean_expression:
  | diff_expression { $1 }
  | boolean_expression OP_CONJUNCTION boolean_expression { LastExpr.conjunction $1 $3 }
  | boolean_expression OP_DISJUNCTION boolean_expression { LastExpr.disjunction $1 $3 }
/* Translate 'a => b' to 'NOT a OR b' */
  | boolean_expression OP_IMPLIES boolean_expression { LastExpr.disjunction (LastExpr.negate $1) $3 }
  | LPAREN boolean_expression RPAREN { $2 }
;

diff_expression:
LASTVar OP_MINUS LASTVar op_bool linear_expression { LastExpr.atomic $1 $3 $4 $5 }
;

linear_expression:
  | NUM {$1}
  | ID {$1}
  | OP_MINUS ID {"-" ^ $2}
  | NUM OP_MUL ID {$1 ^ " * " ^ $3}
  | linear_expression sum_diff linear_expression {$1 ^ $2 ^ $3}

sum_diff:
  | OP_PLUS { " + " }
  | OP_MINUS { " - " }
;

/************************************************************/
op_bool:
/************************************************************/
  | OP_L   { LastExpr.CmpOp.Lt }
  | OP_LEQ { LastExpr.CmpOp.Le }
  | OP_EQ  { LastExpr.CmpOp.Eq }
  | OP_GEQ { LastExpr.CmpOp.Ge }
  | OP_G   { LastExpr.CmpOp.Gt }
;

/************************************************************/
/** OPTIONAL SYMBOLS */
/************************************************************/

and_opt:
  | OP_CONJUNCTION {}
  | {}
;

comma_opt:
  | COMMA { }
  | { }
;


semicolon_opt:
  | SEMICOLON { }
  | { }
;
