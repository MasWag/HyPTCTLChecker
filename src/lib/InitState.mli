open ParsingStructure

type t = ParsingStructure.init_definition
(* Use our initial dummy location instead of the original location*)
val use_our_initial_location: t -> t
val to_string: t -> string
val to_initial_location_map: t -> int -> (string, string) Hashtbl.t
val duplicate: int -> string list -> t -> t
val default_initialization: t
