type name = string
type var = int
type term = Var of name | Sem_var of var | Constr of name * term list
type subst = (var * term) list
type state = subst * var

let is_list = function
  | Constr ("Nil", []) | Constr ("Cons", [ _; _ ]) -> true
  | _ -> false

let rec print_list fmt =
  let open Format in
  let rec print_l fmt = function
    | Constr ("Nil", []) -> ()
    | Constr ("Cons", [ hd; Constr ("Nil", []) ]) ->
        fprintf fmt "; %a" print_term hd
    | Constr ("Cons", [ hd; tl ]) ->
        fprintf fmt "; %a%a" print_term hd print_l tl
    | t -> fprintf fmt "; %a" print_term t
    (* | t -> fprintf fmt " | %a" print_term t *)
  in

  function
  | Constr ("Nil", []) -> fprintf fmt "[]"
  | Constr ("Cons", [ hd; tl ]) -> fprintf fmt "[%a%a]" print_term hd print_l tl
  | _ -> failwith "Unexpected term."

and print_term fmt =
  let open Format in
  function
  | (Constr ("Nil", []) | Constr ("Cons", [ _; _ ])) as t -> print_list fmt t
  | Sem_var v -> fprintf fmt "_.%d" v
  | Constr (name, []) -> fprintf fmt "%s" name
  | Constr (name, args) ->
      fprintf fmt "%s(%a)" name
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") print_term)
        args
  | Var v -> fprintf fmt "%s" v

let print_subst fmt =
  let open Format in
  function
  | [] -> fprintf fmt "{ }"
  | subst ->
      let subst = List.sort (fun (v1, _) (v2, _) -> Int.compare v1 v2) subst in
      fprintf fmt "{ @[@\n%a@]@\n}"
        (pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt ";@\n")
           (fun fmt (var, term) -> fprintf fmt "_.%d <- %a" var print_term term))
        subst

let constant n = Constr (n, [])
let nil = constant "Nil"
let cons hd tl = Constr ("Cons", [ hd; tl ])
let list f l = List.fold_right (fun hd tl -> cons (f hd) tl) l nil

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

let rec apply_subst : subst -> term -> term =
 fun subst term ->
  match term with
  | Constr (n, args) -> Constr (n, List.map (apply_subst subst) args)
  | Sem_var v -> (
      match List.assoc_opt v subst with
      | Some t -> apply_subst subst t
      | None -> term)
  | Var n ->
      failwith
      @@ Format.sprintf "Unexpected syntax variable '%s' in apply_subst" n
