open ParsingStructure

let actions_to_string actions =
  match actions with
  | [] -> ""
  | _ -> "\tactions: " ^ (String.concat ", " actions) ^ ";"

let transition_to_string (guard, parsed_seq_code_bloc, sync, target) =
  let guardStr = String.concat " & " guard in
  let syncStr = 
    match sync with
    | Sync action_name -> " sync " ^ action_name
    | NoSync -> "" in
  let parsedInstructionStr = 
    match parsed_seq_code_bloc with
    | [] -> ""
    | _ -> " do { " ^ (String.concat "; " parsed_seq_code_bloc) ^ " }" in
  "when " ^ guardStr ^ syncStr ^ parsedInstructionStr ^ " goto " ^ target ^ ";"

let location_to_string location =
  let invariantStr = String.concat " & " location.invariant in
    (* if urgent *)
  let locationLine = 
    begin
      match location.urgency with
      | Parsed_location_urgent -> "\turgent loc "
      | Parsed_location_nonurgent -> "\tloc "
    end
    ^ location.name ^ ": invariant " ^ invariantStr ^
      begin
        if location.stopped = [] then ""
        else " stop{" ^ (String.concat ", " location.stopped) ^ "}"
      end
    ^ "\n\t\t" in
  let transitionLines =
    List.map transition_to_string location.transitions in
  locationLine ^ String.concat "\n\t\t" transitionLines
let%test _ = location_to_string {
                 invariant = ["x_2 <= wcet_2"];
                 urgency = Parsed_location_nonurgent;
                 acceptance = Parsed_location_nonaccepting;
                 label = [];
                 (* We keep the stopped clocks *)
                 stopped = ["x_1"];
                 name = "w_1_ok_2";
                 transitions = [];
               } = "\tloc w_1_ok_2: invariant x_2 <= wcet_2 stop{x_1}\n\t\t"

let automaton_to_string (name, actions, locations) =
  String.concat "\n"
    (("automaton " ^ name) ::
       (actions_to_string actions) ::
         (List.map location_to_string locations)) ^ "\nend"

let to_string (automata: ParsingStructure.parsed_automaton list) =
  List.map automaton_to_string automata |>
    String.concat "\n"

let addIndexToVariable parameters idx guard =
  let pattern = Str.regexp "[a-zA-Z][a-zA-Z0-9_]*" in
  (* Add reserved terms *)
  let parameters = "True" :: "False" :: parameters in
  Str.global_substitute pattern (fun var -> 
      let matched_string = (Str.matched_string var) in
      if List.mem matched_string parameters then
        matched_string
      else matched_string ^ "_" ^ (string_of_int idx)) guard
let%test _ = addIndexToVariable ["param"] 2 "x > 10" = "x_2 > 10"
let%test _ = addIndexToVariable ["param"] 2 "x - y < param" = "x_2 - y_2 < param"
let%test _ = addIndexToVariable ["param"] 2 "True" = "True"

(* This must be applied before duplicating the transitions for observers *)
let addIndex automaton parameters idx =
  let (name, actions, locations) = automaton in
  let newName = name ^ "_" ^ (string_of_int idx) in
  let newActions =
    List.map (fun action -> action ^ "_" ^ (string_of_int idx)) actions in
  let addIndexToVariable = addIndexToVariable parameters idx in
  let newLocations =
    List.map (fun location ->
        let newInvariant = List.map addIndexToVariable location.invariant in
        let newTransitions =
          List.map (fun (guard, parsed_seq_code_bloc, sync, target) ->
              let newGuard = List.map addIndexToVariable guard in
              let newParsedSeqCodeBloc = List.map addIndexToVariable parsed_seq_code_bloc in
              let newSync = 
                match sync with
                | Sync action_name -> Sync (addIndexToVariable action_name)
                | NoSync -> NoSync in
              (newGuard, newParsedSeqCodeBloc, newSync, target)) location.transitions in
        {
          invariant = newInvariant;
          urgency = location.urgency;
          acceptance = location.acceptance;
          label = List.map addIndexToVariable location.label;
          stopped = List.map addIndexToVariable location.stopped;
          name = location.name;
          transitions = newTransitions
      }) locations in
  (newName, newActions, newLocations)

module Transition = struct
  let splitTransition labelMap source (guard, parsed_seq_code_bloc, sync, target) =
    let originalLabel = Hashtbl.find labelMap source in
    let targetLabel = Hashtbl.find labelMap target in
    (* The labels not in originalLabel but in targetLabel *)
    let newLabel =
      List.filter (fun label -> not (List.mem label originalLabel)) targetLabel in
    let newUpdates = List.map (fun label -> "status_" ^ label ^ " := CHANGED_UP") newLabel in
    (guard, "changed := True" :: parsed_seq_code_bloc @ newUpdates, sync,  source ^ "_" ^ target)

  let apply_count_mods (count_mods: (string, int) Hashtbl.t) (guard, parsed_seq_code_bloc, sync, target) =
    let new_parsed_seq_code_bloc =
      List.map (fun instruction ->
          let pattern = Str.regexp "\([a-zA-Z][a-zA-Z0-9_]*\)[ \t\n]*:=[ \t\n]*\([^ \t\n].*\)" in
          Str.global_substitute pattern (fun matched_string ->
              let var = Str.matched_group 1 matched_string in
              let expr = Str.matched_group 2 matched_string in
              if Hashtbl.mem count_mods var then
                var ^ " := mod(" ^ expr ^ ", " ^ (string_of_int (Hashtbl.find count_mods var)) ^ ")"
              else Str.matched_string matched_string) instruction) parsed_seq_code_bloc in
    (guard, new_parsed_seq_code_bloc, sync, target)
  let%test _ = let count_mods = Hashtbl.create 1 in
               Hashtbl.add count_mods "x" 3;
               let _, instruction::[], _, _ = apply_count_mods count_mods (["True"], ["x := x + 1"], Sync "notify", "target") in
               apply_count_mods count_mods (["True"], ["x := x + 1"], Sync "notify", "target") = (["True"], ["x := mod(x + 1, 3)"], Sync "notify", "target")
end

module Location = struct
  let splitTransitions labelMap location =
    let splittedTransitions = List.map (Transition.splitTransition labelMap location.name) location.transitions in
    let relevantLabels = (Hashtbl.find labelMap location.name) :: (List.map (fun (_, _, _, target) -> Hashtbl.find labelMap target) location.transitions) |>
                           List.flatten |>
                           List.sort_uniq compare in
    let resetRelevantLabels = List.map (fun label -> "status_" ^ label ^ " := UNCHANGED") relevantLabels in
    let targets =
      List.map (fun (_, _, _, target) -> target) location.transitions |>
        List.sort_uniq compare in
    let dummySelfLoop = 
      (["changed"], [], Sync "notify", location.name) in
    { (* The original location*)
      invariant = location.invariant;
      urgency = location.urgency;
      acceptance = location.acceptance;
      stopped = location.stopped;
      label = location.label;
      name = location.name;
      transitions = dummySelfLoop :: splittedTransitions
    } :: List.map (fun target -> (* The dummy location*)
             let commitRising =
               let originalLabel = Hashtbl.find labelMap location.name in
               let targetLabel = Hashtbl.find labelMap target in
               let risingLabel =
                 List.filter (fun label -> not (List.mem label originalLabel)) targetLabel in
               List.map (fun rising -> ["COUNT_" ^ rising ^ " := " ^ "COUNT_" ^ rising ^ " + 1"; "c_LAST_" ^ rising ^ " := 0"]) risingLabel |>
                 List.concat in
             { 
               invariant = ["True"];
               urgency = Parsed_location_urgent;
               acceptance = location.acceptance;
               label = location.label;
               (* We keep the stopped clocks *)
               stopped = location.stopped;
               name = location.name ^ "_" ^ target;
               transitions = [(["changed"], "changed := False" :: resetRelevantLabels @ commitRising, Sync "notify", target)]
           }) targets

  let apply_count_mods count_mods locations =
    List.map (fun location ->
        let newTransitions = List.map (Transition.apply_count_mods count_mods) location.transitions in
        { location with transitions = newTransitions }) locations
end

let apply_count_mods_autom count_mods (name, actions, locations) =
  (name, actions, Location.apply_count_mods count_mods locations)

module Automaton = struct
  type t =
    | RawAutomaton of ParsingStructure.parsed_automaton
    | Computed of ParsingStructure.parsed_automaton * (string, string list) Hashtbl.t

  let from_autom automaton = RawAutomaton automaton

  let precompute automaton =
    match automaton with
    | RawAutomaton automaton ->
       let (_, _, locations) = automaton in
       let automata_hash = Hashtbl.create (List.length locations) in
       List.iter (fun location ->
           Hashtbl.add automata_hash location.name location.label) locations;
       Computed (automaton, automata_hash)
    | Computed _ -> automaton

  let duplicate size parameters automata=
    match automata with
    | RawAutomaton automaton ->
       (* We use 1-origin indices *)
       List.init size (fun idx -> addIndex automaton parameters (idx + 1)) |>
         List.map (fun automaton -> RawAutomaton automaton)
    | Computed (automaton, hash) ->
       List.init size (fun idx -> addIndex automaton parameters (idx + 1)) |>
         List.map (fun automaton -> Computed (automaton, hash))

  let rec splitTransitions automaton =
    match automaton with
    | RawAutomaton _ ->
       splitTransitions (precompute automaton)
    | Computed (automaton, hash) ->
       let (name, actions, locations) = automaton in       
       let newLocations = List.map (Location.splitTransitions hash) locations in
       (* "notify" is the special action in our construction *)
       Computed ((name, "notify" :: actions, List.concat newLocations), hash)

  let labels automaton =
    begin
    match automaton with
    | RawAutomaton automaton -> 
       let (_, _, locations) = automaton in
       List.map (fun location -> location.label) locations
    | Computed (_, hash) -> Hashtbl.fold (fun _ value acc -> value :: acc) hash []
    end
    |> List.concat

  let apply_count_mods count_mods automaton =
    match automaton with
    | RawAutomaton automaton ->
       RawAutomaton (apply_count_mods_autom count_mods automaton)
    | Computed (automaton, hash) ->
       Computed (apply_count_mods_autom count_mods automaton, hash)

  let rec make_dummy_initial_location map automaton =
    let labels = labels automaton in
    match automaton with
    | Computed (automaton, _) -> make_dummy_initial_location map (RawAutomaton automaton)
    | RawAutomaton (automaton) ->
         let (name, actions, locations) = automaton in
         let initial_location = Hashtbl.find map name in
         let new_actions = "initialize" :: actions in
         let new_location = {
             invariant = ["True"];
             urgency = Parsed_location_urgent;
             acceptance = Parsed_location_nonaccepting;
             label = labels;
             name = "initial";
             stopped = [];
             transitions = [(["True"], [], Sync "initialize", initial_location)]
           } in
         RawAutomaton (name, new_actions, new_location :: locations)

  let makeLastClockVarDefinition automaton =
    let labels = labels automaton in
    LastVars.fromLabels labels |>
      LastVars.makeClockVarDefinition

  let makeCountsVarDefinition automaton =
    let labels = labels automaton in
    Counts.fromLabels labels |>
      Counts.makeCountsVarDefinition

  let makeStatusVarDefinition automaton =
    let labels = labels automaton in
    List.map (fun label -> "status_" ^ label) labels |>
      ImitatorVariableDefinition.boolean_variables

  let to_string automaton =
    match automaton with
    | RawAutomaton automaton -> automaton_to_string automaton
    | Computed (automaton, _) -> automaton_to_string automaton  

  let makeLastClockVarInit automaton =
    let labels = labels automaton in
    LastVars.fromLabels labels |>
      LastVars.makeClockVarInit

  let makeCountsVarInit automaton =
    let labels = labels automaton in
    Counts.fromLabels labels |>
      Counts.makeCountsVarInit

  let makeStatusVarInit automaton =
    let labels = labels automaton in
    List.map (fun label -> 
        Parsed_discrete_predicate
          ("status_" ^ label, "UNCHANGED")) labels
end
