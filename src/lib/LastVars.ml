open ParsingStructure

type t = string list
let empty = []

(** Construct from a list of last variables *)
let fromList vars =
  (* Throw exception if some variables' name does not start with "LAST_" *)
  if List.exists (fun var -> not (String.starts_with ~prefix:"LAST_" var)) vars then
    raise (Invalid_argument "Variable name does not start with 'LAST_'")
  else
    vars

let map = List.map
let mem = List.mem
let filter = List.filter

let fromLabels =
  List.map (fun x -> "LAST_" ^ x)

let append left right =
  List.append left right |>
    List.sort_uniq compare

let makeClockVarDefinition lastVars =
  List.map (fun x -> "c_" ^ x) lastVars |>
    ImitatorVariableDefinition.clock_variables

let makeClockVarInit lastVars =
  List.map (fun x -> Parsed_linear_predicate ("c_" ^ x ^ " = 0")) lastVars

(* The number of the necessary self-composition *)
let compositionSize vars =
  let pattern = (Str.regexp "LAST_[^_ \t]+_\([0-9]+\)") in
  List.map (fun name -> 
      Str.replace_first pattern "\1" name) vars |>
    List.sort_uniq (String.compare) |>
    List.map int_of_string |>
    List.fold_left max 0

let to_string lastVars =
  String.concat ", " lastVars

let nonEmptyPowerset lastVars =
  let rec powerset lastVars =
    match lastVars with
    | [] -> [[]]
    | x :: xs ->
       let ps = powerset xs in
       ps @ List.map (fun ss -> x :: ss) ps in
  List.filter (fun x -> x <> []) (powerset lastVars)
