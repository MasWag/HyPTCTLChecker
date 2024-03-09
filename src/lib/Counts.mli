open ImitatorVariableDefinition
open InitState

(* The module related to COUNT formulas *)
type t
(* Parse the property to get the COUNT variables *)
val parse : string -> t
val fromLabels : string list -> t
(* Generate the variable definition in IMITATOR from the property *)
val makeCountsVarDefinition : t -> ImitatorVariableDefinition.t
(* Generate the variable initialization in IMITATOR from the property *)
val makeCountsVarInit: t -> InitState.t
(* The number of the necessary self-composition *)
val compositionSize : t -> int
