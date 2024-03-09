(* LAST_foo - LAST_bar cmp num *)
module CmpOp: sig
  type t = Lt | Le | Ge | Gt | Eq | Ne
  val to_string: t -> string
  val negate: t -> t
end

type t
val atomic: string -> string -> CmpOp.t -> string -> t
val conjunction: t -> t -> t
val disjunction: t -> t -> t
val negate: t -> t
val flatten: t -> t
val to_dnf: t -> t
val to_string: t -> string
val disjuncts: t -> t list

val to_last_vars: t -> LastVars.t
val to_edge: string -> LastVars.t -> LastVars.t -> ParsingStructure.sync -> string -> t -> bool option -> bool -> ParsingStructure.transition
(* Make an observer as an automaton *)
val to_automaton: string -> t -> Model.Automaton.t
