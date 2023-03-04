open GT

(*****************************************************************************************)

type name = string
type var = int
type term = Var of name | Sem_var of var | Constr of name * term list
type subst = (var * term) list
type state = subst * var

type goal =
  | Unify of term * term
  | Fresh of name list * goal
  | Disj of goal * goal
  | Conj of goal * goal
  | Invoke of name * term list

type relation = name * (name list * goal)
type environment = relation list

type stream =
  | Nil
  | Leaf of goal * state
  | Mplus of stream * stream
  | Bind of stream * goal

type step_trace = state option * stream
type trace = step_trace list

(*****************************************************************************************)
module Printing = struct
  open Format

  let rec print_term fmt = function
    | Sem_var v -> fprintf fmt "_.%d" v
    | Constr (name, []) -> fprintf fmt "%s" name
    | Constr (name, args) ->
        fprintf fmt "%s(%a)" name
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") print_term)
          args
    | Var v -> fprintf fmt "%s" v

  let rec print_goal fmt = function
    | Unify (t1, t2) -> fprintf fmt "%a === %a" print_term t1 print_term t2
    | Fresh (vs, g) ->
        fprintf fmt "fresh (%a) %a"
          (pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt " ")
             (fun fmt -> fprintf fmt "%s"))
          vs print_goal g
    | Disj (g1, g2) -> fprintf fmt "%a ||| %a" print_goal g1 print_goal g2
    | Conj (g1, g2) -> fprintf fmt "%a &&& %a" print_goal g1 print_goal g2
    | Invoke (name, args) ->
        fprintf fmt "%s %a" name
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "%, ") print_term)
          args

  let print_subst fmt = function
    | [] -> fprintf fmt "{ }"
    | subst ->
        let subst =
          List.sort (fun (v1, _) (v2, _) -> Int.compare v1 v2) subst
        in
        fprintf fmt "{ @[@\n%a@]@\n}"
          (pp_print_list
             ~pp_sep:(fun fmt () -> fprintf fmt ";@\n")
             (fun fmt (var, term) ->
               fprintf fmt "_.%d <- %a" var print_term term))
          subst

  let print_state fmt (subst, counter) =
    fprintf fmt "%a,@\nVar counter: %d" print_subst subst counter

  let rec print_stream fmt = function
    | Nil -> fprintf fmt "%s" "nil"
    | Leaf (g, st) ->
        fprintf fmt "< @[@\n%a,@\n%a@]@\n>" print_goal g print_state st
    | Mplus (s1, s2) ->
        fprintf fmt "mplus@\n  @[%a@\n%a@]" print_stream s1 print_stream s2
    | Bind (s, g) ->
        fprintf fmt "bind@\n  @[%a@\n%a@]" print_stream s print_goal g

  let print_step_trace fmt = function
    | None, s -> fprintf fmt "No answer, @\n%a@\n" print_stream s
    | Some a, s ->
        fprintf fmt "Answer:@\n  @[%a,@] @\n%a@\n" print_state a print_stream s

  let print_step_trace_with_index fmt (index, trace) =
    fprintf fmt
      "----------------------------------------------@\n@\nStep %d:@\n  @[%a@]"
      index print_step_trace trace

  let print_trace fmt trace =
    pp_print_list
      ~pp_sep:(fun fmt () -> fprintf fmt "@\n")
      print_step_trace_with_index fmt
    @@ List.mapi (fun i t -> (i, t)) trace
end
(*****************************************************************************************)

let ( === ) t1 t2 = Unify (t1, t2)
let ( ||| ) g1 g2 = Disj (g1, g2)
let ( &&& ) g1 g2 = Conj (g1, g2)
let fresh vs g = Fresh (vs, g)
let invoke n args = Invoke (n, args)
let constant n = Constr (n, [])
let nil = constant "Nil"
let cons hd tl = Constr ("Cons", [ hd; tl ])
let list f l = List.fold_right (fun hd tl -> cons (f hd) tl) l nil
let init_state : state = ([], 0)
let init_stream : goal -> stream = fun g -> Leaf (g, init_state)

(*****************************************************************************************)

let rec subst_in_goal : name -> term -> goal -> goal =
 fun var substituted_term goal ->
  let rec subst_in_term var substituted_term term =
    match term with
    | Var var' when var = var' -> substituted_term
    | Var _ | Sem_var _ -> term
    | Constr (name, args) ->
        Constr (name, List.map (subst_in_term var substituted_term) args)
  in
  let subst_in_term = subst_in_term var substituted_term in
  let subst_in_goal = subst_in_goal var substituted_term in
  match goal with
  | Unify (t1, t2) -> Unify (subst_in_term t1, subst_in_term t2)
  | Disj (g1, g2) -> Disj (subst_in_goal g1, subst_in_goal g2)
  | Conj (g1, g2) -> Conj (subst_in_goal g1, subst_in_goal g2)
  | Invoke (n, args) -> Invoke (n, List.map subst_in_term args)
  | Fresh (vars, _) when List.mem var vars -> goal
  | Fresh (vars, g) -> Fresh (vars, subst_in_goal g)

let apply : name -> term list -> environment -> goal =
 fun name args env ->
  match List.assoc_opt name env with
  | Some (arg_names, body) ->
      let act_len = List.length args in
      let exp_len = List.length arg_names in
      if act_len = exp_len then
        List.fold_right2 subst_in_goal arg_names args body
      else
        failwith
        @@ Format.sprintf
             "Incorrect amount of arguments in relation '%s' (actual: %d, \
              expected: %d)"
             name act_len exp_len
  | None ->
      failwith
      @@ Format.sprintf "Relation '%s' is missing in the environment" name

(*****************************************************************************************)

let rec walk : term -> subst -> term =
 fun term subst ->
  match term with
  | Sem_var v -> (
      match List.assoc_opt v subst with Some t -> walk t subst | None -> term)
  | Constr _ -> term
  | Var n ->
      failwith @@ Format.sprintf "Unexpected syntax variable '%s' in walk" n

let rec occurs_check : var -> term -> subst -> bool =
 fun var term subst ->
  match walk term subst with
  | Sem_var var' -> var = var'
  | Constr (_, args) ->
      List.fold_left
        (fun acc arg -> acc || occurs_check var arg subst)
        false args
  | Var n ->
      failwith
      @@ Format.sprintf "Unexpected syntax variable '%s' in occurs check" n

let extend : var -> term -> subst -> subst option =
 fun var term subst ->
  if occurs_check var term subst then None else Some ((var, term) :: subst)

let rec unify : term -> term -> subst -> subst option =
 fun term1 term2 subst ->
  let unify_option term1 term2 = function
    | None -> None
    | Some subst -> unify term1 term2 subst
  in
  match (walk term1 subst, walk term2 subst) with
  | Sem_var x, Sem_var y when x = y -> Some subst
  | term, Sem_var x | Sem_var x, term -> extend x term subst
  | Constr (name1, args1), Constr (name2, args2) when name1 = name2 ->
      List.fold_right2 unify_option args1 args2 (Some subst)
  | Constr _, Constr _ -> None
  | Var n, _ | _, Var n ->
      failwith @@ Format.sprintf "Unexpected syntax variable '%s' in unify" n

(*****************************************************************************************)
let print_trace :
    (environment -> int -> stream -> trace) ->
    environment ->
    int ->
    goal ->
    unit =
 fun eval env n goal ->
  let res = eval env n @@ init_stream goal in
  Format.printf "%a\n%!" Printing.print_trace res
