open ParsingStructure

type t = ParsingStructure.init_definition

let is_loc_init init =
  match init with
  | Parsed_loc_assignment _ -> true
  | _ -> false

let is_discrete_init init =
  match init with
  | Parsed_discrete_predicate _ -> true
  | _ -> false

let is_continuous_init init =
  match init with
  | Parsed_linear_predicate _ -> true
  | _ -> false

let use_our_initial_location inits =
  List.map (fun init ->
      match init with
      | Parsed_loc_assignment (automaton, _) ->
         Parsed_loc_assignment (automaton, "initial")
      | _ -> init) inits

let to_initial_location_map inits size =
  let map = Hashtbl.create (List.length inits) in
  List.init size (fun idx ->
      List.iter (fun init ->
          match init with
          | Parsed_loc_assignment (automaton, loc) ->
             Hashtbl.add map (automaton ^ "_" ^ string_of_int (idx + 1)) loc
          | _ -> ()) inits) |> ignore;
  map

module Assignment = struct
  type t = ParsingStructure.parsed_init_state_predicate
  let to_string assignment =
    match assignment with
    | Parsed_loc_assignment (automaton, loc) -> "\t\tloc[" ^ automaton ^ "] := " ^ loc ^ ","
    | Parsed_linear_predicate predicate -> "\t\t& " ^ predicate
    | Parsed_discrete_predicate (variable, expr) -> "\t\t" ^ variable ^ " := " ^ expr ^ ","
end

let to_string inits =
  let locInits =
    List.filter is_loc_init inits in
  let discreteInits =
    List.filter is_discrete_init inits in
  let continuousInits =
    List.filter is_continuous_init inits in
  let header = "init := {" in
  let footer = "}" in
  let headerDiscrete = "\tdiscrete =" in
  let footerDiscrete = "\t;" in
  let headerContinuous = "\tcontinuous =" in
  let footerContinuous = "\t;" in
  if inits = [] then
    ""
  else
    let discreteInitString =
      if locInits = [] && discreteInits = [] then
        ""
      else
        let discreteInitString =
          (List.map Assignment.to_string locInits) @ (List.map Assignment.to_string discreteInits) |>
            List.sort_uniq compare in
        (headerDiscrete::discreteInitString) @ [footerDiscrete] |>
          String.concat "\n" in

    let continuousInitString =
      if continuousInits = [] then
        ""
      else
        let continuousInitsString =
          List.map Assignment.to_string continuousInits |>
            List.sort_uniq compare in
        (headerContinuous::continuousInitsString) @ [footerContinuous] |>
          String.concat "\n" in

    String.concat "\n" [header; discreteInitString; continuousInitString; footer]

let duplicate size parameters inits =
  (* Internal constants should not be modified *)
  let parameters = "changed" :: "UNCHANGED" :: "CHANGED_UP" :: parameters in
  let addIndexToVariable idx guard =
    let pattern = Str.regexp "[a-zA-Z][a-zA-Z0-9_]*" in
    Str.global_substitute pattern (fun var -> 
        let matched_string = Str.matched_string var in
        if List.mem matched_string parameters then
          matched_string
        else matched_string ^ "_" ^ (string_of_int idx)) guard in
  List.map (fun init ->
      match init with
      | Parsed_loc_assignment (automaton, loc) ->
         List.init size (fun idx -> 
             Parsed_loc_assignment (addIndexToVariable (idx + 1) automaton, loc))
      | Parsed_discrete_predicate (variable, expr) ->
         List.init size (fun idx -> 
             Parsed_discrete_predicate (addIndexToVariable (idx + 1) variable, addIndexToVariable (idx + 1) expr))
      | Parsed_linear_predicate guard ->
         List.init size (fun idx -> 
             Parsed_linear_predicate (addIndexToVariable (idx + 1) guard))) inits |>
    List.concat
            
let default_initialization =
  [Parsed_discrete_predicate ("changed", "False")]
