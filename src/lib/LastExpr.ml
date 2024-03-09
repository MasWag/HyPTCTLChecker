open ParsingStructure
open ImitatorVariableDefinition

(* LAST_foo - LAST_bar cmp num *)
module CmpOp = struct
  type t = Lt | Le | Ge | Gt | Eq | Ne
  let to_string cmpOp =
    match cmpOp with
    | Lt -> "<"
    | Le -> "<="
    | Ge -> ">="
    | Gt -> ">"
    | Eq -> "="
    | Ne -> "<>"

  let negate cmpOp =
    match cmpOp with
    | Lt -> Ge
    | Le -> Gt
    | Ge -> Lt
    | Gt -> Le
    | Eq -> Ne
    | Ne -> Eq

end

type t =
  DiffExpr of string * string * CmpOp.t * string
| Conjunction of t list
| Disjunction of t list

let atomic first second operator threshold =
  DiffExpr (first, second, operator, threshold)

let conjunction left right =
  let left_internal =
    match left with
    | Conjunction list -> Some list
    | _ -> None in
  let right_internal =
    match right with
    | Conjunction list -> Some list
    | _ -> None in
  begin
    if Option.is_none left_internal && Option.is_none right_internal then
      [left; right]
    else if Option.is_none left_internal then
      left :: (Option.get right_internal)
    else if Option.is_none right_internal then
      right :: (Option.get left_internal)
    else
      List.append (Option.get left_internal) (Option.get right_internal)
  end |>
    fun x -> Conjunction x

let disjunction left right =
  let left_internal =
    match left with
    | Disjunction list -> Some list
    | _ -> None in
  let right_internal =
    match right with
    | Disjunction list -> Some list
    | _ -> None in
  begin
    if Option.is_none left_internal && Option.is_none right_internal then
      [left; right]
    else if Option.is_none left_internal then
      left :: (Option.get right_internal)
    else if Option.is_none right_internal then
      right :: (Option.get left_internal)
    else
      List.append (Option.get left_internal) (Option.get right_internal)
  end |>
    fun x -> Disjunction x

let rec negate expr =
  match expr with
  | DiffExpr (first, second, op, threshold) -> DiffExpr (first, second, CmpOp.negate op, threshold)
  | Conjunction (list) -> Disjunction (List.map negate list)
  | Disjunction (list) -> Conjunction (List.map negate list)

let rec to_string expr =
  match expr with
  | DiffExpr (first, second, cmpOp, string) -> first ^ " - " ^ second ^ " " ^ (CmpOp.to_string cmpOp) ^ " " ^ string
  | Conjunction lst -> 
     List.map to_string lst |>
       List.map (fun expr -> "(" ^ expr ^ ")") |>
       String.concat " && "
  | Disjunction lst -> 
     List.map to_string lst |>
       List.map (fun expr -> "(" ^ expr ^ ")") |>
       String.concat " || "

let is_conjunction expr =
  match expr with
  | Conjunction _ -> true
  | _ -> false

let is_disjunction expr =
  match expr with
  | Disjunction _ -> true
  | _ -> false

let rec flatten expr =
  match expr with
  | DiffExpr _ -> expr
  | Conjunction lst -> 
     List.map flatten lst |>
       List.map (fun x -> match x with
                          | Conjunction lst -> lst
                          | _ -> [x]) |>
       List.concat |>
       fun x -> Conjunction x
  | Disjunction lst ->
     List.map flatten lst |>
       List.map (fun x -> match x with
                          | Disjunction lst -> lst
                          | _ -> [x]) |>
       List.concat |>
       fun x -> Disjunction x

let to_list expr =
    match expr with
    | DiffExpr _ -> [expr]
    | Conjunction lst -> lst
    | Disjunction lst -> lst

let combine expr_list =
  let head =
    List.hd expr_list |>
      List.map (fun x -> [x]) in
  let tail =
    List.tl expr_list in
  let accumulate1 (product: t list list) (item: t) : t list list =
    List.map (fun (lst: t list) ->
        item :: lst) product in
  let accumulate (product: t list list) (item: t list) : t list list =
    List.map (accumulate1 product) item |>
      List.concat in
  List.fold_left accumulate head tail

let rec to_dnf expr =
  match flatten expr with
  | DiffExpr _ -> Disjunction [expr]
  | Conjunction lst -> 
     let sub_fmls = 
       List.map to_dnf lst |>
         List.map to_list in
     if List.length sub_fmls = 1 then
       Conjunction (List.hd sub_fmls)
     else
       combine sub_fmls |>
         (List.map (fun x -> Conjunction x)) |>
            fun x -> Disjunction x
  | Disjunction lst ->
     let sub_fmls = 
       Disjunction (List.map to_dnf lst) in
     flatten sub_fmls

let disjuncts expr =
  match to_dnf expr with
  | Disjunction lst -> lst
  | _ -> raise (Invalid_argument "disjuncts")

let to_last_vars expr =
  let rec from_expr expr =
    match expr with
    | DiffExpr (first, second, _, _) -> [first; second]
    | Conjunction lst -> List.concat (List.map from_expr lst)
    | Disjunction lst -> List.concat (List.map from_expr lst) in
  List.sort_uniq compare (from_expr expr) |>
    LastVars.fromList

let to_guard all changed disjunct only_clock =
    let pattern = (Str.regexp "LAST_\([^_ \t]+_[0-9]+\)") in
    let to_status var =
      "status_" ^ (Str.replace_first pattern "\1" var) in
    let to_clock var =
      "c_" ^ var in
    let unchanged =
      LastVars.filter (fun x -> not (LastVars.mem x changed)) all in
    let changed_guards =
      LastVars.map (fun x -> to_status x ^ " = CHANGED_UP " ) changed in
    let unchanged_guards =
      LastVars.map (fun x -> to_status x ^ " = UNCHANGED " ) unchanged in
    let clock_guards =
      List.map (fun diffExpr ->
          match diffExpr with
          | DiffExpr (first, second, cmpOp, threshold) ->
             if LastVars.mem first changed && LastVars.mem second changed then
               "0 " ^ CmpOp.to_string cmpOp ^ " " ^ threshold
             else if LastVars.mem first changed then
               "-" ^ (to_clock second) ^ " " ^ CmpOp.to_string cmpOp ^ " " ^ threshold
             else if LastVars.mem second changed then
               (to_clock first) ^ " " ^ CmpOp.to_string cmpOp ^ " " ^ threshold
             else
               ""
          | _ -> raise (Invalid_argument "to_guard")) (to_list disjunct) in
    if only_clock then
      clock_guards
    else
      changed_guards @ unchanged_guards @ clock_guards

let to_edge name all changed action target disjunct satisfied only_clock =
  let guard = to_guard all changed disjunct only_clock in
  let reset =
    match satisfied with
    | Some(true) -> [name ^ " := True"; "initialized := True"]
    | Some(false) -> [name ^ " := False"; "initialized := True"]
    | None -> [] in
  (guard, reset, action, target)

let to_parsed_automaton name expr =
  let actions = ["notify"; "initialize"] in
  let lastVars: LastVars.t = to_last_vars expr in
  let raisedVars = LastVars.nonEmptyPowerset lastVars in
  let satisfiedDisjuncts = disjuncts expr in
  let violatedDisjuncts = disjuncts (negate expr) in
  let initial_location =
    let satisfiedEdges =
      List.map (fun disjunct ->
          to_edge name lastVars lastVars (Sync "initialize") "checking" disjunct (Some true) true
        ) satisfiedDisjuncts in
    let violatedEdges =
      List.map (fun disjunct ->
          to_edge name lastVars lastVars (Sync "initialize") "checking" disjunct (Some false) true
        ) violatedDisjuncts in
    {
      name = "initial";
      urgency = Parsed_location_urgent;
      acceptance = Parsed_location_nonaccepting;
      invariant = ["True"];
      stopped = [];
      label = [];
      transitions = satisfiedEdges @ violatedEdges;
    } in
  let checking_location =
    (* No one changed: no change to the satisfaction *)
    let noChangeEdges = [to_edge name lastVars LastVars.empty (Sync "notify") "checking" (Conjunction []) None false] in
    let satisfiedEdges =
      List.map (fun disjunct ->
          List.map (fun raised ->
              to_edge name lastVars raised (Sync "notify") "checking" disjunct (Some true) false
            ) raisedVars
        ) satisfiedDisjuncts |> List.concat in
    let violatedEdges =
      List.map (fun disjunct ->
          List.map (fun raised ->
              to_edge name lastVars raised (Sync "notify") "checking" disjunct (Some false) false
            ) raisedVars
        ) violatedDisjuncts |> List.concat in
    {
      name = "checking";
      urgency = Parsed_location_nonurgent;
      acceptance = Parsed_location_nonaccepting;
      invariant = ["True"];
      stopped = [];
      label = [];
      transitions = noChangeEdges @ satisfiedEdges @ violatedEdges;
    } in
  ("observer_" ^ name, actions, [initial_location; checking_location])

let to_automaton name expr =
  to_parsed_automaton name expr |>
    Model.Automaton.from_autom
