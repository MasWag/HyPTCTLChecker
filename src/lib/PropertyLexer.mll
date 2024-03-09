{
open PropertyParser

(* OCaml style comments *)
let comment_depth = ref 0;;

let line=ref 1;;

let reservedWords = [
  (* Keywords *)
  ("let", PropertyParser.LET);
  ("in", PropertyParser.IN);
  ("var", PropertyParser.VAR);
  ("mod", PropertyParser.MOD);
  ("use", PropertyParser.USE);
  ("global_time", PropertyParser.GLOBAL_TIME);
]
}

rule token = parse
    (* ignore spacing and newline characters *)
     [' ' '\009' '\012' '\n']+     { token lexbuf }
	(* Embedded property: The part <* ... *> is ignored.*)
    | "<*"_+"*>" as lxm { EMBEDDED lxm }
    |  ['\n']             { line := !line + 1 ; token lexbuf }     (* skip new lines *)
	| [' ' '\t']         { token lexbuf }     (* skip blanks *)

	(* OCaml style comments *)
	| "(*"
		{ comment_depth := 1;
		comment_ocaml lexbuf;
		token lexbuf }

	(* LAST variable *)
    | "LAST_"['a'-'z''A'-'Z''0'-'9']+'_'['0'-'9']+ as lxm { LASTVar lxm }

	(* COUNT variable *)
    | "COUNT_"['a'-'z''A'-'Z''0'-'9']+'_'['0'-'9']+ as lxm { COUNTVar lxm }

	(* Numbers *)
	| '-'?['0'-'9']*'.'?['0'-'9']+ as lxm { NUM lxm }

	(* Comparison operators *)
	| "<="             { OP_LEQ }
	| ">="             { OP_GEQ }
	| '<'              { OP_L }
	| '='              { OP_EQ }
	| "<>"             { OP_NEQ }
	| '>'              { OP_G }

	(* DELIMITAR *)
	| ":"             { COLON }
	| ";"             { SEMICOLON }

	(* Boolean operators *)
	| '&'              { OP_CONJUNCTION }
	| "&&"             { OP_CONJUNCTION }
	| '|'              { OP_DISJUNCTION }
	| "||"             { OP_DISJUNCTION }
	| "=>"             { OP_IMPLIES }

	(* Arithmetic operators *)
	| '+'              { OP_PLUS }
	| '-'              { OP_MINUS }
	| '/'              { OP_DIV }
	| '*'              { OP_MUL }

	(* Parentheses and the like *)
	| '('              { LPAREN }
	| ')'              { RPAREN }
	| '{'              { LBRACE }
	| '}'              { RBRACE }
	| '['              { LSQBRA }
	| ']'              { RSQBRA }

    | ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
      { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      _ -> PropertyParser.ID id
     }

	| eof              { EOF}
	| _ { failwith("Unexpected symbol '" ^ (Lexing.lexeme lexbuf) ^ "' at line " ^ string_of_int !line)}



(* OCaml style comments *)
and comment_ocaml = parse
    "(*"  { incr comment_depth; comment_ocaml lexbuf }
  | "*)"  { decr comment_depth;
            if !comment_depth == 0 then () else comment_ocaml lexbuf }
  | eof
    { failwith "End of file inside a comment in property." }
  | '\n'  { line := !line + 1 ; comment_ocaml lexbuf }
  | _     { comment_ocaml lexbuf }
