/************************************************************
 *
 *                       IMITATOR
 *
 * Laboratoire Spécification et Vérification (ENS Cachan & CNRS, France)
 * Université Sorbonne Paris Nord, LIPN, CNRS, France
 * Université de Lorraine, CNRS, Inria, LORIA, Nancy, France
 *
 * Module description: Parser for the input model
 *
 * File contributors : Étienne André, Jaime Arias, Benjamin Loillier, Laure Petrucci
 * Created           : 2009/09/07
 *
 ************************************************************/


%{
open ParsingStructure;;
open ImitatorUtilities;;

(* let parse_error _ = *)
(* 	let symbol_start = symbol_start () in *)
(* 	let symbol_end = symbol_end () in *)
(* 	raise (ParsingError (symbol_start, symbol_end)) *)
(* ;; *)


(*** TODO (Jaime): is it included twice ? ***)
let include_list = ref [];;

(* let add_parsed_model_to_parsed_model_list parsed_model_list parsed_model = *)
(* 	let merged_controllable_actions : ParsingStructure.parsed_controllable_actions = match parsed_model.model.controllable_actions, parsed_model_list.model.controllable_actions with *)
(* 			| Parsed_no_controllable_actions, Parsed_no_controllable_actions *)
(* 				-> Parsed_no_controllable_actions *)

(* 			| Parsed_no_controllable_actions, Parsed_controllable_actions action_names *)
(* 			| Parsed_controllable_actions action_names, Parsed_no_controllable_actions *)
(* 				-> Parsed_controllable_actions action_names *)

(* 			| Parsed_controllable_actions action_names_1, Parsed_controllable_actions action_names_2 *)
(* 				-> Parsed_controllable_actions (OCamlUtilities.list_append action_names_1 action_names_2) *)

(* 			| Parsed_uncontrollable_actions action_names_1, Parsed_uncontrollable_actions action_names_2 *)
(* 				-> Parsed_uncontrollable_actions (OCamlUtilities.list_append action_names_1 action_names_2) *)

(* 			| Parsed_no_controllable_actions, Parsed_uncontrollable_actions action_names *)
(* 			| Parsed_uncontrollable_actions action_names, Parsed_no_controllable_actions *)
(* 				-> Parsed_uncontrollable_actions action_names *)

(* 			| Parsed_uncontrollable_actions u_action_names, Parsed_controllable_actions c_action_names *)
(* 			| Parsed_controllable_actions c_action_names, Parsed_uncontrollable_actions u_action_names *)
(* 				-> *)
(* 				(\*** WARNING (2023/07/10): should be an error ***\) *)
(* 				print_warning ("The submodels define contradictory controllable list of actions (" ^ (OCamlUtilities.string_of_list_of_string_with_sep ", " c_action_names) ^ ") AND uncontrollable list of actions (" ^ (OCamlUtilities.string_of_list_of_string_with_sep ", " u_action_names) ^ "); the model is ill-formed and its behavior is unspecified!"); *)
(* 				Parsed_controllable_actions c_action_names *)
(* 		in *)

(* 	{ *)
(*                 model = *)
(*                 { *)
(*                         controllable_actions  = merged_controllable_actions; *)
(*                         variable_declarations = List.append parsed_model.model.variable_declarations parsed_model_list.model.variable_declarations; *)
(*                         fun_definitions       = List.append parsed_model.model.fun_definitions parsed_model_list.model.fun_definitions; *)
(*                         automata              = List.append parsed_model.model.automata parsed_model_list.model.automata; *)
(*                         init_definition       = List.append parsed_model.model.init_definition parsed_model_list.model.init_definition; *)
(*                 }; *)
(*                 template_definitions  = List.append parsed_model.template_definitions parsed_model_list.template_definitions; *)
(*                 template_calls        = List.append parsed_model.template_calls parsed_model_list.template_calls; *)
(* 	} *)
(* ;; *)

(* let unzip l = List.fold_left *)
(* 	add_parsed_model_to_parsed_model_list *)
(* 	{ *)
(*                 model = *)
(*                 { *)
(*                         controllable_actions  = Parsed_no_controllable_actions; *)
(*                         variable_declarations = []; *)
(*                         fun_definitions       = []; *)
(*                         automata              = []; *)
(*                         init_definition       = []; *)
(*                 }; *)
(*                 template_definitions  = []; *)
(*                 template_calls        = []; *)
(* 	} *)
(* 	(List.rev l) *)
(* ;; *)

%}

%token <string> VARS
%token <string> NUM
%token <string> BINARYWORD
%token <string> NAME
%token <string> TYPE
/* %token <string> STRING */
%token <ParsingStructure.parsed_model_with_templates> INCLUDE

%token OP_PLUS OP_MINUS OP_MUL OP_DIV
%token OP_L OP_LEQ OP_EQ OP_NEQ OP_GEQ OP_G OP_ASSIGN

%token LPAREN RPAREN LBRACE RBRACE LSQBRA RSQBRA
%token APOSTROPHE COLON COMMA DOUBLEDOT OP_CONJUNCTION OP_DISJUNCTION OP_IMPLIES SEMICOLON

%token
  CT_ACCEPTING CT_ACTION CT_ACTIONS CT_ARRAY CT_AUTOMATON
	CT_BEGIN CT_BINARY_WORD CT_BOOL
	CT_CLOCK CT_CONSTANT CT_CONTINUOUS CT_CONTROLLABLE
	CT_DO CT_DONE CT_DOWNTO
	CT_ELSE CT_END
	CT_FALSE CT_FLOW CT_FOR CT_FROM CT_FUN
	CT_GOTO
	CT_IF CT_IN CT_INFINITY CT_INIT CT_INSIDE CT_INSTANTIATE CT_INT CT_INVARIANT CT_IS
	CT_LOC 	CT_LABEL
	CT_NOT
	CT_PARAMETER
	CT_RATIONAL CT_RETURN
	CT_STOP CT_SYNC CT_SYNCLABS
	CT_TEMPLATE CT_THEN CT_TO CT_TRUE
	CT_UNCONTROLLABLE CT_URGENT
	CT_VAR CT_VOID
	CT_WAIT CT_WHEN CT_WHILE
	/*** NOTE: just to forbid their use in the input model and property ***/
	CT_NOSYNCOBS CT_OBSERVER CT_OBSERVER_CLOCK CT_SPECIAL_RESET_CLOCK_NAME
    CT_BUILTIN_FUNC_RATIONAL_OF_INT /* CT_POW CT_SHIFT_LEFT CT_SHIFT_RIGHT CT_FILL_LEFT CT_FILL_RIGHT
    CT_LOG_AND CT_LOG_OR CT_LOG_XOR CT_LOG_NOT CT_ARRAY_CONCAT CT_LIST_CONS */ CT_LIST CT_STACK CT_QUEUE


%token EOF

%right OP_ASSIGN
%right OP_EQ

%left OP_IMPLIES           /* lowest precedence */
%left OP_DISJUNCTION /* CT_OR */
%left OP_CONJUNCTION       /* medium precedence */
%left DOUBLEDOT            /* high precedence */
%nonassoc CT_NOT           /* highest precedence */

%left OP_PLUS OP_MINUS     /* lowest precedence */
%left OP_MUL OP_DIV        /* highest precedence */


%start main             /* the entry point */
%type <ImitatorVariableDefinition.t list * ParsingStructure.parsed_automaton list * parsed_init_state_predicate list> main
(* %type <(var_type * (string * string option) list) list> main *)
(* %type <ParsingStructure.parsed_model_with_templates> main *)
%%

/************************************************************/
main:
  variables_declarations automata init_definition_option end_opt EOF {($1, $2, $3)}
(* 	controllable_actions_option include_file_list variables_declarations decl_fun_lists template_defs automata template_calls init_definition_option *)
(* 	end_opt EOF *)
(* 	{ *)
(* 		let controllable_actions = $1 in *)
(* 		let declarations         = $3 in *)
(* 		let fun_definitions      = $4 in *)
(* 		let template_definitions = $5 in *)
(* 		let automata             = $6 in *)
(* 		let template_calls       = $7 in *)
(* 		let init_definition      = $8 in *)

(* 		let main_model = *)
(* 		{ *)
(*                         model = *)
(*                         { *)
(*                                 controllable_actions  = controllable_actions; *)
(*                                 variable_declarations = declarations; *)
(*                                 fun_definitions       = fun_definitions; *)
(*                                 automata              = automata; *)
(*                                 init_definition       = init_definition; *)
(*                         }; *)
(*                         template_definitions  = template_definitions; *)
(*                         template_calls        = template_calls; *)
(* 		} *)
(* 		in *)
(* 		let included_model = unzip !include_list in *)

(* 		(\* Return the parsed model *\) *)
(* 		add_parsed_model_to_parsed_model_list included_model main_model *)
(* 	} *)
(* ; *)

end_opt:
	| CT_END { }
	| { }
;


/************************************************************
  CONTROLLABLE ACTIONS
************************************************************/
controllable_actions_option:
	| CT_CONTROLLABLE CT_ACTIONS COLON name_list SEMICOLON { Parsed_controllable_actions $4 }
	| CT_UNCONTROLLABLE CT_ACTIONS COLON name_list SEMICOLON { Parsed_uncontrollable_actions $4 }
	| { Parsed_no_controllable_actions }
;


/************************************************************
  VARIABLE DECLARATIONS
************************************************************/

/************************************************************/

variables_declarations:
	| CT_VAR decl_var_lists { $2 }
	| { []}
;


/************************************************************
	INCLUDES
************************************************************/
include_file_list:
	| include_file include_file_list  { $1 :: $2 }
	| { [] }
;

include_file:
	| INCLUDE SEMICOLON { $1 }
;

/************************************************************/

/************************************************************/

decl_var_lists:
	| decl_var_list COLON CT_INT SEMICOLON decl_var_lists { (ImitatorVariableDefinition.int_variables $1) :: $5 }
	| decl_var_list COLON CT_BOOL SEMICOLON decl_var_lists { (ImitatorVariableDefinition.boolean_variables $1) :: $5 }
	| decl_var_list COLON CT_CLOCK SEMICOLON decl_var_lists { (ImitatorVariableDefinition.clock_variables $1) :: $5 }
	| decl_var_list COLON CT_PARAMETER SEMICOLON decl_var_lists { (ImitatorVariableDefinition.parameter_variables $1) :: $5 }
	| { [] }
;

/************************************************************/

decl_var_list:
	| NAME comma_opt { [$1] }
	| NAME OP_EQ boolean_expression comma_opt { [($1 ^ " = " ^ $3)] }
	| NAME COMMA decl_var_list { $1 :: $3 }
	| NAME OP_EQ boolean_expression COMMA decl_var_list { ($1 ^ " = " ^ $3) :: $5 }
;

/************************************************************/

(* template_var_type: *)
(*   | var_type { Regular_type $1 } *)
(*   | CT_ACTION { Template_action_var } *)
(* ; *)

(* var_type: *)
(* 	| CT_CLOCK { Var_type_clock } *)
(* 	| CT_CONSTANT { Var_type_discrete (Dt_number Dt_rat) } *)
(* 	| CT_PARAMETER { Var_type_parameter } *)
(* 	| var_type_discrete { Var_type_discrete $1 } *)
(* ; *)

(* var_type_discrete: *)
(*     | var_type_discrete_number { Dt_number $1 } *)
(*     | CT_VOID { Dt_void } *)
(*     | CT_BOOL { Dt_bool } *)
(*     | CT_BINARY_WORD LPAREN NUM RPAREN { Dt_bin (int_of_string $3) } *)
(*     | var_type_discrete_array { $1 } *)
(*     | var_type_discrete_list { $1 } *)
(*     | var_type_discrete_stack { $1 } *)
(*     | var_type_discrete_queue { $1 } *)
(* ; *)

(* var_type_discrete_array: *)
(*   | var_type_discrete CT_ARRAY LPAREN NUM RPAREN { Dt_array ($1, int_of_string $4) } *)
(* ; *)

(* var_type_discrete_list: *)
(*   | var_type_discrete CT_LIST { Dt_list $1 } *)
(* ; *)

(* var_type_discrete_stack: *)
(*   | var_type_discrete CT_STACK { Dt_stack $1 } *)
(* ; *)

(* var_type_discrete_queue: *)
(*   | var_type_discrete CT_QUEUE { Dt_queue $1 } *)
(* ; *)

(* var_type_discrete_number: *)
(*     | CT_RATIONAL { Dt_rat } *)
(*     | CT_INT { Dt_int } *)
(* ; *)

/************************************************************/

(* decl_fun_lists: *)
(* 	| decl_fun_nonempty_list { List.rev $1 } *)
(* 	| { [] } *)
(* ; *)

(* /* Declaration function list */ *)
(* decl_fun_nonempty_list: *)
(*   | decl_fun_def { [$1] } *)
(*   | decl_fun_nonempty_list decl_fun_def { $2 :: $1 } *)
(* ; *)

(* /* Function definition */ *)
(* decl_fun_def: *)
(*   | CT_FUN NAME LPAREN fun_parameter_list RPAREN COLON var_type_discrete CT_BEGIN seq_code_bloc return_opt CT_END *)
(*   { *)
(*     { *)
(*       name = $2; *)
(*       parameters = List.rev $4; *)
(*       return_type = $7; *)
(*       body = $9, $10; *)
(*     } *)
(*   } *)
(* ; *)

(* return_opt: *)
(*   | CT_RETURN boolean_expression semicolon_opt { Some $2 } *)
(*   | { None } *)
(* ; *)

fun_parameter_list:
  | { [] }
  | fun_parameter_nonempty_list { $1 }
;

/* Function parameters list (separated by whitespace) */
fun_parameter_nonempty_list:
  | NAME COLON TYPE { [(($1, Parsing.symbol_start ()), $3)] }
  | fun_parameter_nonempty_list COMMA NAME COLON TYPE { (($3, Parsing.symbol_start ()), $5) :: $1 }
;

seq_code_bloc:
  | { [] }
  | seq_code_bloc_nonempty_list { $1 }
;

/* Bloc of code (instructions, declarations, conditionals, loops) */
seq_code_bloc_nonempty_list:
  | instruction semicolon_or_comma seq_code_bloc_nonempty_list { $1 :: $3 }
  (* | control_structure seq_code_bloc_nonempty_list { $1 :: $2 } *)
  | instruction semicolon_or_comma_opt { [$1] }
  (* | control_structure { [$1] } *)
;

semicolon_or_comma_opt:
  | {}
  | semicolon_or_comma {}
;

instruction:
  /* local declaration */
  | CT_VAR NAME COLON TYPE OP_EQ boolean_expression { "var " ^ $2 ^ ": " ^ $4 ^ " = " ^ $6 }
  /* assignment */
  | update_without_deprecated { $1 }
  /* instruction without return */
  | boolean_expression { $1 }

;

/** Normal updates without deprecated (avoid parsing errors on function)*/
update_without_deprecated:
	| parsed_scalar_or_index_update_type OP_ASSIGN boolean_expression { $1 ^ " := " ^ $3 }
;

/* Variable or variable access */
parsed_scalar_or_index_update_type:
  | NAME { $1 }
  | parsed_scalar_or_index_update_type LSQBRA arithmetic_expression RSQBRA { $1 ^ "[" ^ $3 ^ "]" }
;

(*
control_structure:
  /* for loop */
  | CT_FOR NAME CT_FROM arithmetic_expression loop_dir arithmetic_expression CT_DO seq_code_bloc CT_DONE { Parsed_for_loop (($2, Parsing.symbol_start ()), $4, $6, $5, $8) }
  /* while loop */
  | CT_WHILE boolean_expression CT_DO seq_code_bloc CT_DONE { Parsed_while_loop ($2, $4) }
  /* conditional */
  | CT_IF boolean_expression CT_THEN seq_code_bloc CT_END { Parsed_if ($2, $4, None) }
  | CT_IF boolean_expression CT_THEN LPAREN seq_code_bloc RPAREN CT_END { Parsed_if ($2, $5, None) }
  | CT_IF boolean_expression CT_THEN seq_code_bloc CT_ELSE seq_code_bloc CT_END { Parsed_if ($2, $4, Some $6) }
  | CT_IF boolean_expression CT_THEN LPAREN seq_code_bloc RPAREN CT_ELSE LPAREN seq_code_bloc RPAREN CT_END { Parsed_if ($2, $5, Some $9) }
;

loop_dir:
  | CT_TO { Parsed_for_loop_up }
  | CT_DOWNTO { Parsed_for_loop_down }
;

/************************************************************/

/************************************************************
  TEMPLATES
************************************************************/

/************************************************************/

template_defs:
  | template_def template_defs { $1 :: $2 }
  | { [] }
;

/************************************************************/

template_def:
  | CT_TEMPLATE NAME LPAREN template_parameter_list RPAREN prolog locations CT_END {
      let body = ($6, $7) in
      { template_name       = $2
      ; template_parameters = List.rev $4
      ; template_body       = body
      }
  }
;

/************************************************************/

template_parameter_list:
  | { [] }
  | template_parameter_nonempty_list { $1 }
;

/************************************************************/

/* TODO: var_type correctly represents the types accepted by templates? */
template_parameter_nonempty_list:
  | NAME COLON template_var_type { [($1, $3)] }
  | template_parameter_nonempty_list COMMA NAME COLON template_var_type { ($3, $5) :: $1 }
;

/************************************************************/

template_calls:
  | template_call template_calls { $1 :: $2 }
  | { [] }
;

/************************************************************/

template_call:
	| CT_INSTANTIATE NAME OP_ASSIGN NAME LPAREN template_args_list RPAREN SEMICOLON
	{
		($2, $4, List.rev $6)
	}
;

template_args_list:
  | { [] }
  | template_args_nonempty_list { $1 }
;

template_args_nonempty_list:
  | template_args_nonempty_list COMMA template_args_elem { $3 :: $1 }
  | template_args_elem { [$1] }
;

template_args_elem:
  | NAME     { Arg_name $1    }
  | NUM      { Arg_num  $1    }
  | CT_TRUE  { Arg_bool true  }
  | CT_FALSE { Arg_bool false }
;

/************************************************************/

/************************************************************
  AUTOMATA
************************************************************/

/************************************************************/

 *)

automata:
	| automaton automata { $1 :: $2 }
	(* | include_file automata { include_list := $1 :: !include_list; $2 } *)
	| { [] }
;

/************************************************************/

automaton:
	| CT_AUTOMATON NAME prolog locations CT_END
	{
		($2, $3, $4)
	}
;

/************************************************************/

prolog:
	| actions_declarations { $1 }
	| { [] }
;

/************************************************************/


/************************************************************/

actions_declarations:
	| CT_ACTIONS COLON name_list SEMICOLON { $3 }
	/** NOTE: deprecated since 3.4 */
	| CT_SYNCLABS COLON name_list SEMICOLON {
			print_warning ("The syntax `synclabs` is deprecated since version 3.4; please use `actions` instead.");
	$3 }
;

/************************************************************/

name_list:
	| name_nonempty_list { $1 }
	| { [] }
;

/************************************************************/

name_nonempty_list:
	NAME COMMA name_nonempty_list { $1 :: $3}
	| NAME comma_opt { [$1] }
;

/************************************************************/

locations:
	location locations { $1 :: $2}
	| { [] }
;

/************************************************************/


location:
	| loc_urgency_accepting_type NAME COLON while_or_nothing guard_nonlinear_convex_predicate stopwatches_and_flow_opt COLON label transitions {
		let urgency, accepting = $1 in
		let name = $2 in
        let stopped, _ = $6 in
        let labels = $8 in
		{
			(* Name *)
			name		= name;
			(* Urgent or not? *)
			urgency		= urgency;
			(* Accepting or not? *)
			acceptance	= accepting;
			(* Invariant *)
			invariant	= $5;
			(* Label *)
			label		= labels;
	        (* List of stopped clocks *)
            stopped		= stopped;
			(* Transitions starting from this location *)
			transitions = $9;
		}
	}
;


loc_urgency_accepting_type:
	| CT_LOC { Parsed_location_nonurgent, Parsed_location_nonaccepting }
	| CT_URGENT CT_LOC { Parsed_location_urgent, Parsed_location_nonaccepting }
	| CT_ACCEPTING CT_LOC { (Parsed_location_nonurgent, Parsed_location_accepting) }
	| CT_URGENT CT_ACCEPTING CT_LOC { (Parsed_location_urgent, Parsed_location_accepting) }
	| CT_ACCEPTING CT_URGENT CT_LOC { (Parsed_location_urgent, Parsed_location_accepting) }
;

while_or_nothing:
	| CT_WHILE {
		print_warning ("The syntax `while [invariant]` is deprecated; you should use `invariant [invariant]` instead.");
		()
		}
	| CT_INVARIANT {}
	| {}
;

label:
  | CT_LABEL LBRACE name_list RBRACE { $3 }
  | { [] }
;

/************************************************************/

stopwatches_and_flow_opt:
	| stopwatches flow { $1, $2 }
	| flow stopwatches { $2, $1 }
	| stopwatches { $1, [] }
	| flow { [], $1 }
	| { [], [] }
;

/************************************************************/

flow:
	| CT_FLOW LBRACE flow_list RBRACE { $3 }
;


/************************************************************/

flow_list:
	| flow_nonempty_list { $1 }
	| { [] }
;

/************************************************************/

flow_nonempty_list:
	| single_flow COMMA flow_nonempty_list { $1 :: $3 }
	| single_flow comma_opt { [$1] }
;

/************************************************************/

single_flow:
	| NAME APOSTROPHE OP_EQ flow_value { ($1, $4) }
;

/************************************************************/

flow_value:
        | rational_linear_expression { Flow_rat_value $1 }
        | NAME { Flow_var $1 }

/************************************************************/

stopwatches:
	| CT_STOP LBRACE name_list RBRACE { $3 }
;

/************************************************************/


transitions:
	| transition transitions { $1 :: $2 }
	| { [] }
;

/************************************************************/

transition:
	| CT_WHEN guard_nonlinear_convex_predicate update_synchronization CT_GOTO NAME SEMICOLON
	{
		let update_list, sync = $3 in
			$2, update_list, sync, $5
	}
;

/************************************************************/

/* A l'origine de 3 conflits ("2 shift/reduce conflicts, 1 reduce/reduce conflict.") donc petit changement */
update_synchronization:
	| { [], NoSync }
	| updates { $1, NoSync }
	| sync_action { [], (Sync $1) }
	| updates sync_action { $1, (Sync $2) }
	| sync_action updates { $2, (Sync $1) }
;

/************************************************************/

updates:
  | CT_DO LBRACE seq_code_bloc RBRACE { $3 }
;

/************************************************************/

sync_action:
	CT_SYNC NAME { $2 }
;

/************************************************************/
/** INIT DEFINITION */
/************************************************************/

init_definition_option:
    | init_definition { $1 }
    | { [ ] }
;

/************************************************************/
/** NEW INIT DEFINITION SECTION from 3.1: SEPARATION OF DISCRETE AND CONTINUOUS */
/************************************************************/

init_definition:
	| CT_INIT OP_ASSIGN LBRACE init_discrete_continuous_definition RBRACE semicolon_opt { $4 }
;

init_discrete_continuous_definition:
    | init_discrete_definition { $1 }
    | init_continuous_definition { $1 }
    | init_discrete_definition init_continuous_definition { $1 @ $2 }
    | init_continuous_definition init_discrete_definition { $2 @ $1 }
;

init_discrete_definition:
    | CT_RATIONAL OP_EQ init_discrete_expression SEMICOLON { $3 }
;

init_continuous_definition:
    | CT_CONTINUOUS OP_EQ init_continuous_expression SEMICOLON { $3 }
;


init_discrete_expression:
	| comma_opt init_discrete_expression_nonempty_list { $2 }
	| { [ ] }
;

init_discrete_expression_nonempty_list :
	| init_discrete_state_predicate COMMA init_discrete_expression_nonempty_list  { $1 :: $3 }
	| init_discrete_state_predicate comma_opt { [ $1 ] }
;

init_discrete_state_predicate:
	| init_loc_predicate { let a,b = $1 in (Parsed_loc_assignment (a,b)) }
	| LPAREN init_discrete_state_predicate  RPAREN { $2 }
	| NAME OP_ASSIGN boolean_expression { Parsed_discrete_predicate ($1, $3) }
;

init_continuous_expression:
	| ampersand_opt init_continuous_expression_nonempty_list { $2 }
	| { [ ] }
;

init_continuous_expression_nonempty_list :
	| init_continuous_state_predicate OP_CONJUNCTION init_continuous_expression_nonempty_list  { $1 :: $3 }
	| init_continuous_state_predicate ampersand_opt { [ $1 ] }
;

init_continuous_state_predicate:
    | LPAREN init_continuous_state_predicate RPAREN { $2 }
    | init_linear_constraint { Parsed_linear_predicate $1 }
;

init_loc_predicate:
	/* loc[my_pta] = my_loc */
	| CT_LOC LSQBRA NAME RSQBRA OP_ASSIGN NAME { ($3, $6) }
	/* my_pta IS IN my_loc */
	| NAME CT_IS CT_IN NAME { ($1, $4) }
;



/************************************************************/
/** ARITHMETIC EXPRESSIONS */
/************************************************************/

arithmetic_expression:
	| arithmetic_term { $1 }
	| arithmetic_expression sum_diff arithmetic_term { $1 ^ " " ^ $2 ^ " " ^ $3 }
;

sum_diff:
  | OP_PLUS { "+" }
  | OP_MINUS { "-" }
;

/* Term over variables and rationals (includes recursion with arithmetic_expression) */
arithmetic_term:
	| arithmetic_factor { $1 }
	/* Shortcut for syntax rational NAME without the multiplication operator */
	(* | NUM NAME { Parsed_product_quotient (Parsed_factor (Parsed_constant ($1)), Parsed_variable ($2, 0), Parsed_mul) } *)
	| arithmetic_term product_quotient arithmetic_factor { $1 ^ " " ^ $2 ^ " " ^$3 }
	| OP_MINUS arithmetic_factor { "-" ^ $2 }
;

product_quotient:
  | OP_MUL { "*" }
  | OP_DIV { "/" }
;

arithmetic_factor:
  | arithmetic_factor LSQBRA arithmetic_expression RSQBRA { $1 ^ "[" ^ $3 ^ "]" }
  | NAME LPAREN function_argument_fol RPAREN { $1 ^ "(" ^ $3 ^ ")" }
  | literal_scalar_constant { $1 }
  (* | literal_non_scalar_constant { $1 } *)
  | NAME { $1 }
  | LPAREN arithmetic_expression RPAREN { "(" ^ $2 ^ ")"}
;

literal_scalar_constant:
  | NUM { $1 }
  | CT_TRUE { "True" }
  | CT_FALSE { "False" }
  (* | binary_word { $1 } *)
;

(*
literal_non_scalar_constant:
  | literal_array { Parsed_sequence ($1, Parsed_array) }
  | CT_LIST LPAREN literal_array RPAREN { Parsed_sequence ($3, Parsed_list) }
  | CT_STACK LPAREN RPAREN { Parsed_sequence ([], Parsed_stack) }
  | CT_QUEUE LPAREN RPAREN { Parsed_sequence ([], Parsed_queue) }
;

literal_array:
  /* Empty array */
  | LSQBRA RSQBRA { [] }
  /* Non-empty array */
  | LSQBRA literal_array_fol RSQBRA { $2 }
;

literal_array_fol:
	| boolean_expression COMMA literal_array_fol { $1 :: $3 }
	| boolean_expression { [$1] }
;

 *)
function_argument_fol:
  | boolean_expression COMMA function_argument_fol { $1 ^ ", " ^ $3 }
  | boolean_expression { $1 }
  | { "" }
;
(*
binary_word:
        BINARYWORD { ParsedValue.Bin_value (BinaryWord.binaryword_of_string $1) }
;
 *)
/************************************************************/
/* LINEAR EXPRESSIONS IN INIT DEFINITIONS */
/************************************************************/

init_linear_constraint:
	| linear_expression relop linear_expression { $1 ^ $2 ^ $3 }
	| CT_TRUE { "True" }
	| CT_FALSE { "False" }
;

/* Linear expression over variables and rationals */
linear_expression:
	| linear_term { $1 }
	| linear_expression OP_PLUS linear_term { $1 ^ " + " ^ $3 }
	| linear_expression OP_MINUS linear_term { $1 ^ " - " ^ $3 }
;

/* Linear term over variables and rationals (no recursion, no division) */
linear_term:
	| NUM { $1 }
	| NUM NAME { $1 ^ " * " ^ $2 }
	| NUM OP_MUL NAME { $1 ^ " * " ^ $3 }
	| OP_MINUS NAME { "-" ^ $2 }
	| NAME { $1 }
	| LPAREN linear_term RPAREN { "(" ^ $2 ^ ")" }
;


/* Linear expression over rationals only */
rational_linear_expression:
	| rational_linear_term { $1 }
	| rational_linear_expression OP_PLUS rational_linear_term { $1 ^ " + " ^ $3 }
	| rational_linear_expression OP_MUL rational_linear_term { $1 ^ " * " ^ $3 }
	| rational_linear_expression OP_DIV rational_linear_term { $1 ^ " / " ^ $3 }
	| rational_linear_expression OP_MINUS rational_linear_term { $1 ^ " - " ^ $3 }
;

/* Linear term over rationals only */
rational_linear_term:
	| NUM { $1 }
	| LPAREN rational_linear_expression RPAREN { "(" ^ $2 ^ ")"}
;

/************************************************************/
/** RATIONALS, LINEAR TERMS, LINEAR CONSTRAINTS AND CONVEX PREDICATES */
/************************************************************/

/* We allow an optional "&" at the beginning of a convex predicate (sometimes useful) */
guard_nonlinear_convex_predicate:
	| ampersand_opt guard_nonlinear_convex_predicate_fol { $2 }
;

guard_nonlinear_convex_predicate_fol:
	/** NOTE: this part of the code is necessary (?) to detect convex constraints (with parameters), BUT forbids the use of Boolean expressions without parentheses */
	| discrete_boolean_expression OP_CONJUNCTION guard_nonlinear_convex_predicate_fol { $1 :: $3 }
	| discrete_boolean_expression { [$1] }
;

/** NOTE: more general than a Boolean expression!! notably includes all expressions */
boolean_expression:
	| discrete_boolean_expression { $1 }
	| boolean_expression OP_CONJUNCTION boolean_expression { "(" ^ $1 ^ ") && (" ^ $3 ^ ")"}
	| boolean_expression OP_DISJUNCTION boolean_expression { "(" ^ $1 ^ ") || (" ^ $3 ^ ")"}
	/* Translate 'a => b' to 'NOT a OR b' */
	| boolean_expression OP_IMPLIES boolean_expression { "(" ^ $1 ^ ") => (" ^ $3 ^ ")"}
;

discrete_boolean_expression:
	| arithmetic_expression { $1 }

	/* Discrete arithmetic expression of the form Expr ~ Expr */
	| discrete_boolean_expression relop discrete_boolean_expression { $1 ^ $2 ^ $3 }

	/* Discrete arithmetic expression of the form 'Expr in [Expr, Expr ]' */
	| arithmetic_expression CT_INSIDE LSQBRA arithmetic_expression semicolon_or_comma arithmetic_expression RSQBRA { $1 ^ " inside [" ^ $4 ^ ", " ^ $6 ^ "]" }

	/* Parsed boolean expression of the form Expr ~ Expr, with ~ in { & | } or not (Expr) */
	| LPAREN boolean_expression RPAREN { "(" ^ $2 ^ ")"}
	| CT_NOT LPAREN boolean_expression RPAREN { "not " ^ $3 }
;

relop:
	| OP_L { " < " }
	| OP_LEQ { " <= " }
	| OP_EQ { " = " }
	| OP_NEQ { " <> " }
	| OP_GEQ { " >=" }
	| OP_G { " > " }
;

/************************************************************/
/** NUMBERS */
/************************************************************/

/************************************************************/
/** MISC. */
/************************************************************/

semicolon_or_comma:
  | SEMICOLON {}
  | COMMA {}
;

comma_opt:
	| COMMA { }
	| { }
;

semicolon_opt:
	| SEMICOLON { }
	| { }
;

ampersand_opt:
	| OP_CONJUNCTION { }
	| { }
;

