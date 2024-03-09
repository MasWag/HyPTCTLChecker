open ParsingStructure

type id = string

(* var COUNT_id: mod int *)
type count_mod =
    CountMod of id * int

(* let id in diffExp *)
type valuation =
    Valuation of id * LastExpr.t

type t =
  | Property of bool * count_mod list * valuation list * string
  | ParsedProperty of bool * count_mod list * valuation list * string * Counts.t * LastVars.t

let parseEmbedded property =
  match property with
  | Property (use_global_time, count_mods, valuations, embedded) ->
     let countVars = Counts.parse embedded in
     let lastVars = 
       List.map (fun vpair -> 
           match vpair with
           | Valuation (_, v) -> (LastExpr.to_last_vars v)) valuations |>
         List.fold_left LastVars.append LastVars.empty in
     ParsedProperty (use_global_time, count_mods, valuations, embedded, countVars, lastVars)
  | ParsedProperty _ -> property

let count_mods property =
  let _count_mods_list =
    match property with
    | Property (_, _count_mods, _, _) -> _count_mods
    | ParsedProperty (_, _count_mods, _, _, _, _) -> _count_mods
  in
  let _count_mods = Hashtbl.create 0 in
  List.iter (fun count_mod ->
      match count_mod with
      | CountMod (id, _mod) ->
         Hashtbl.add _count_mods id _mod) _count_mods_list;
  _count_mods

let global_time property =
  match property with
  | Property (global_time, _, _, _) -> global_time
  | ParsedProperty (global_time, _, _, _, _, _) -> global_time

let valuations property =
  match property with
  | Property (_, _, valuations, _) -> valuations
  | ParsedProperty (_, _, valuations, _, _, _) -> valuations

let ignore_uninitialized property =
  let _ignore_uninitialized property_string =
    let g_expr = Str.regexp "\([^a-zA-Z0-9]\)G\([^a-zA-Z0-9].+\);" in
    let f_expr = Str.regexp "\([^a-zA-Z0-9]\)F\([^a-zA-Z0-9].+\);" in
    let au_expr = Str.regexp "\([^a-zA-Z0-9]\)A\([^a-zA-Z0-9]\(.\|\n\)*[^a-zA-Z0-9]\)U\([^a-zA-Z0-9]\(.\|\n\)*\);" in
    let eu_expr = Str.regexp "\([^a-zA-Z0-9]\)E\([^a-zA-Z0-9]\(.\|\n\)*[^a-zA-Z0-9]\)U\([^a-zA-Z0-9]\(.\|\n\)*\);" in
    let aw_expr = Str.regexp "\([^a-zA-Z0-9]\)A\([^a-zA-Z0-9]\(.\|\n\)*[^a-zA-Z0-9]\)W\([^a-zA-Z0-9]\(.\|\n\)*\);" in
    let ew_expr = Str.regexp "\([^a-zA-Z0-9]\)E\([^a-zA-Z0-9]\(.\|\n\)*[^a-zA-Z0-9]\)W\([^a-zA-Z0-9]\(.\|\n\)*\);" in
    Str.replace_first g_expr "\1G (initialized => \2);" property_string |>
      Str.replace_first f_expr "\1F (not initialized && \2);" |>
      Str.replace_first au_expr "\1A(initialized => \2)U\4;" |>
      Str.replace_first eu_expr "\1E(initialized => \2)U\4;" |>
      Str.replace_first aw_expr "\1A(initialized => \2)W\4;" |>
      Str.replace_first ew_expr "\1E(initialized => \2)W\4;"
  in
  match property with
  | Property (use_global_time, count_mods, valuations, embedded) ->
     Property (use_global_time, count_mods, valuations, _ignore_uninitialized embedded)
  | ParsedProperty (use_global_time, count_mods, valuations, embedded, countVars, lastVars) ->
     ParsedProperty (use_global_time, count_mods, valuations, _ignore_uninitialized embedded, countVars, lastVars)
let%test _ = match ignore_uninitialized (Property (false, [], [], "property := #synth A (\n(mod(COUNT_a_1 - COUNT_a_2, 4) = 0)\n	=>\n    phi_2_satisfied\n	) W (\n		(* phi_3 *)\n		mod(COUNT_a_1 - COUNT_a_2, 4) = 2\n	)\n;\n")) with | Property (_, _, _, embedded) -> embedded = "property := #synth A(initialized =>  (\n(mod(COUNT_a_1 - COUNT_a_2, 4) = 0)\n	=>\n    phi_2_satisfied\n	) )W (\n		(* phi_3 *)\n		mod(COUNT_a_1 - COUNT_a_2, 4) = 2\n	)\n;\n" | ParsedProperty _ -> false
let%test _ = match ignore_uninitialized (Property (false, [], [], "property := #synth E (\n(mod(COUNT_a_1 - COUNT_a_2, 4) = 0)\n	=>\n    phi_2_satisfied\n	) W (\n		(* phi_3 *)\n		mod(COUNT_a_1 - COUNT_a_2, 4) = 2\n	)\n;\n")) with | Property (_, _, _, embedded) -> embedded = "property := #synth E(initialized =>  (\n(mod(COUNT_a_1 - COUNT_a_2, 4) = 0)\n	=>\n    phi_2_satisfied\n	) )W (\n		(* phi_3 *)\n		mod(COUNT_a_1 - COUNT_a_2, 4) = 2\n	)\n;\n" | ParsedProperty _ -> false
let%test _ = match ignore_uninitialized (Property (false, [], [], "property := #synth A (\n(mod(COUNT_a_1 - COUNT_a_2, 4) = 0)\n	=>\n    phi_2_satisfied\n	) U (\n		(* phi_3 *)\n		mod(COUNT_a_1 - COUNT_a_2, 4) = 2\n	)\n;\n")) with | Property (_, _, _, embedded) -> embedded = "property := #synth A(initialized =>  (\n(mod(COUNT_a_1 - COUNT_a_2, 4) = 0)\n	=>\n    phi_2_satisfied\n	) )U (\n		(* phi_3 *)\n		mod(COUNT_a_1 - COUNT_a_2, 4) = 2\n	)\n;\n" | ParsedProperty _ -> false
let%test _ = match ignore_uninitialized (Property (false, [], [], "property := #synth E (\n(mod(COUNT_a_1 - COUNT_a_2, 4) = 0)\n	=>\n    phi_2_satisfied\n	) U (\n		(* phi_3 *)\n		mod(COUNT_a_1 - COUNT_a_2, 4) = 2\n	)\n;\n")) with | Property (_, _, _, embedded) -> embedded = "property := #synth E(initialized =>  (\n(mod(COUNT_a_1 - COUNT_a_2, 4) = 0)\n	=>\n    phi_2_satisfied\n	) )U (\n		(* phi_3 *)\n		mod(COUNT_a_1 - COUNT_a_2, 4) = 2\n	)\n;\n" | ParsedProperty _ -> false
let%test _ = match ignore_uninitialized (Property (false, [], [], "property := #synth A F (oFoG);\n")) with | Property (_, _, _, embedded) -> embedded = "property := #synth A F (not initialized &&  (oFoG));\n" | ParsedProperty _ -> false
let%test _ = match ignore_uninitialized (Property (false, [], [], "property := #synth A G (oFoG);\n")) with | Property (_, _, _, embedded) -> embedded = "property := #synth A G (initialized =>  (oFoG));\n" | ParsedProperty _ -> false
let%test _ = match ignore_uninitialized (Property (false, [], [], "property := #synth E F (oFoG);\n")) with | Property (_, _, _, embedded) -> embedded = "property := #synth E F (not initialized &&  (oFoG));\n" | ParsedProperty _ -> false
let%test _ = match ignore_uninitialized (Property (false, [], [], "property := #synth E G (oFoG);\n")) with | Property (_, _, _, embedded) -> embedded = "property := #synth E G (initialized =>  (oFoG));\n" | ParsedProperty _ -> false

let embedded property =
  match property with
  | Property (_, _, _, embedded) -> embedded
  | ParsedProperty (_, _, _, embedded, _, _) -> embedded

let rec makeCountsVarDefinition property =
  match property with
  | Property _ ->
     parseEmbedded property |>
       makeCountsVarDefinition
  | ParsedProperty (_, _, _, _, countVars, _) ->
     Counts.makeCountsVarDefinition countVars

let rec makeCountsVarInit property =
  match property with
  | Property _ ->
     parseEmbedded property |>
       makeCountsVarInit
  | ParsedProperty (_, _, _, _, countVars, _) ->
     Counts.makeCountsVarInit countVars

let rec makeLastClockVarDefinition property =
  match property with
  | Property _ ->
     parseEmbedded property |>
       makeLastClockVarDefinition
  | ParsedProperty (_, _, _, _, _, lastVars) ->
     LastVars.makeClockVarDefinition lastVars

let rec makeLastClockVarInit property =
  match property with
  | Property _ ->
     parseEmbedded property |>
       makeLastClockVarInit
  | ParsedProperty (_, _, _, _, _, lastVars) ->
     LastVars.makeClockVarInit lastVars

let rec compositionSize property =
  match property with
  | Property _ ->
     parseEmbedded property |>
       compositionSize
  | ParsedProperty (_, _, _, _, countVars, lastVars) ->
     max (Counts.compositionSize countVars)
       (LastVars.compositionSize lastVars)

let to_observer property =
  let valuations = valuations property in
  List.map (fun valuation ->
      match valuation with
      | Valuation (name, expr) ->
         LastExpr.to_automaton name expr |>
           Model.Automaton.to_string) valuations |>
    String.concat "\n"

let observerVariableDeclaration property =
  let valuations = valuations property in
  List.map (fun valuation ->
      match valuation with
      | Valuation (name, _) ->
         name) valuations |>
    ImitatorVariableDefinition.boolean_variables

let observerInit property =
  let valuations = valuations property in
  List.map (fun valuation ->
      match valuation with
      | Valuation (name, _) ->
         Parsed_loc_assignment ("observer_" ^ name, "initial")
    ) valuations
