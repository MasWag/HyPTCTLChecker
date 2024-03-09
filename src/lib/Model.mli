open ParsingStructure

(* val location_to_string : ParsingStructure.parsed_location -> string *)

val automaton_to_string: ParsingStructure.parsed_automaton -> string

val to_string: ParsingStructure.parsed_automaton list -> string

module Automaton: sig
  type t
  val from_autom: ParsingStructure.parsed_automaton -> t
  val to_string: t -> string
  val precompute: t -> t
  val duplicate: int -> string list -> t -> t list
  val splitTransitions: t -> t
  val make_dummy_initial_location: (string, string) Hashtbl.t -> t -> t
  val apply_count_mods: (string, int) Hashtbl.t -> t -> t
  val makeLastClockVarDefinition: t -> ImitatorVariableDefinition.t
  val makeCountsVarDefinition: t -> ImitatorVariableDefinition.t
  val makeStatusVarDefinition: t -> ImitatorVariableDefinition.t
  val makeLastClockVarInit: t -> InitState.t
  val makeCountsVarInit: t -> InitState.t
  val makeStatusVarInit: t -> InitState.t
end
