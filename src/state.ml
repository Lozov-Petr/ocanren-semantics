open Substitution

type state = subst * var

let print_state fmt (subst, counter) =
  Format.fprintf fmt "%a,@\nVar counter: %d" print_subst subst counter

let init_state : state = ([], 9)
