(* The module related to COUNT formulas *)

open ParsingStructure
open ImitatorVariableDefinition

type t = string list
(* Extract the COUNT variables from the property *)
let parse property =
  let pattern =  Str.regexp "COUNT_[^_ \t\n]+_[^_ \t\n,]+" in
  (* Get all the substrings matching the regex pattern *)
  let all_matches s regex =
    let rec aux pos acc =
      try
        (* Search for the regex from the current position *)
        let found = Str.search_forward regex s pos in
        (* Extract the matching substring *)
        let match_str = Str.matched_string s in
        (* Continue searching from the end of the current match *)
        aux (found + String.length match_str) (match_str :: acc)
      with Not_found -> acc
    in
    List.rev (aux 0 [])
  in
  let vars = all_matches property pattern in
  List.sort_uniq (String.compare) vars
let%test _ = parse "    COUNT_SUBMITTED1_1 = COUNT_SUBMITTED2_2
    ) U (" = ["COUNT_SUBMITTED1_1"; "COUNT_SUBMITTED2_2"]

let fromLabels labels =
  List.map (fun label -> "COUNT_" ^ label) labels

(* Generate the variable definition in IMITATOR from the property *)
let makeCountsVarDefinition vars =
  ImitatorVariableDefinition.int_variables vars

(* Generate the variable definition in IMITATOR from the property *)
let makeCountsVarInit vars =
  List.map (fun name -> Parsed_discrete_predicate (name, "0")) vars

(* The number of the necessary self-composition *)
let compositionSize vars =
  let pattern = (Str.regexp "COUNT_[^_ \t]+_\([0-9]+\)") in
  List.map (fun name -> 
      Str.replace_first pattern "\1" name) vars |>
    List.sort_uniq (String.compare) |>
    List.map int_of_string |>
    List.fold_left max 0
