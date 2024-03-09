type t
val empty: t
val fromList: string list -> t
val fromLabels: string list -> t
val map: (string -> 'a) -> t -> 'a list
val mem: string -> t -> bool
val filter: (string -> bool) -> t -> t
val append: t -> t -> t
(* Generate the clocck variable definition in IMITATOR from the property *)
val makeClockVarDefinition : t -> ImitatorVariableDefinition.t
(* Generate the clock variable initialization in IMITATOR from the property *)
val makeClockVarInit: t -> InitState.t
(* The number of the necessary self-composition *)
val compositionSize : t -> int
val nonEmptyPowerset: t -> t list
