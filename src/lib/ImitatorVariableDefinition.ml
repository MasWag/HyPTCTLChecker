(* The type declaration *)
type t =
  | Int of string list
  | Boolean of string list
  | Clock of string list
  | Parameter of string list

(* Functions to construct variable definitions *)
let int_variables names = Int names
let boolean_variables names = Boolean names
let clock_variables names = Clock names
let parameter_variables names = Parameter names

(* Printing functions *)
let to_string definitions =
  match definitions with
  |  Int (name::names) ->
      let namesStr = String.concat ",\n" (List.sort_uniq compare (name::names)) in
      namesStr ^ "\n\t: int;"
  |  Boolean (name::names) ->
      let namesStr = String.concat ",\n" (List.sort_uniq compare (name::names)) in
      namesStr ^ "\n\t: bool;"
  | Clock (name::names) ->
     let namesStr = String.concat ",\n" (List.sort_uniq compare (name::names)) in
     namesStr ^ "\n\t: clock;"
  | Parameter (name::names) ->
     let namesStr = String.concat ",\n" (List.sort_uniq compare (name::names)) in
     namesStr ^ "\n\t: parameter;"
  | _ -> ""
let%test _ = to_string (int_variables []) = ""
let%test _ = to_string (int_variables ["foo"; "bar"]) = "bar,\nfoo\n\t: int;"
let%test _ = to_string (boolean_variables ["foo"; "bar"]) = "bar,\nfoo\n\t: bool;"
let%test _ = to_string (clock_variables ["foo"; "bar"]) = "bar,\nfoo\n\t: clock;"
let%test _ = to_string (parameter_variables ["foo"; "bar"]) = "bar,\nfoo\n\t: parameter;"

let definition_string definitions =
  let each_str = List.map to_string definitions in
  "var\n" ^
    String.concat "\n" each_str
let%test _ = definition_string ([int_variables ["foo"; "bar"]; int_variables ["piyo"]; parameter_variables ["hoge";"fuga"]]) = "var\nbar,\nfoo\n\t: int;\npiyo\n\t: int;\nfuga,\nhoge\n\t: parameter;"

(* Duplicate the variables for self-composition *)
let addIndex names idx =
  let pattern = Str.regexp "\([^ \t\n]+\)" in
  List.map (fun name ->
      (Str.replace_first pattern ("\1_" ^ string_of_int (idx + 1)) name)) names
let%test _ = addIndex ["foo"; "bar = 10"] 0 = ["foo_1"; "bar_1 = 10"]
let duplicate_variables size variables =
  match variables with
  | Int lst -> 
     Int (List.init size (addIndex lst) |> List.concat)
  | Boolean lst -> 
     Boolean (List.init size (addIndex lst) |> List.concat)
  | Clock lst ->
     Clock (List.init size (addIndex lst) |> List.concat)
  (* We do not duplicate parameters *)
  | Parameter lst ->
     Parameter lst

let duplicate_variables_list size variables_list =
  List.map (duplicate_variables size) variables_list

let internal_variables = 
  [boolean_variables ["UNCHANGED = False"; "CHANGED_UP = True"; "changed"; "initialized"]]

(* Extract the name of parameters *)
let parameters (definitions: t list): string list =
  let pattern = Str.regexp "\([^ \t\n]+\).*" in
  List.fold_left (fun acc def ->
      match def with
      | Parameter names -> (List.map (Str.replace_first pattern "\1") names) @ acc
      | _ -> acc) [] definitions
