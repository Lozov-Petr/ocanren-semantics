open Substitution

type goal =
  | Unify of term * term
  | Fresh of name list * goal
  | Disj of goal * goal
  | Conj of goal * goal
  | Invoke of name * term list

type relation = name * (name list * goal)
type environment = relation list

let ( === ) t1 t2 = Unify (t1, t2)
let ( ||| ) g1 g2 = Disj (g1, g2)
let ( &&& ) g1 g2 = Conj (g1, g2)
let fresh vs g = Fresh (vs, g)
let invoke n args = Invoke (n, args)

let rec print_goal fmt =
  let open Format in
  function
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
