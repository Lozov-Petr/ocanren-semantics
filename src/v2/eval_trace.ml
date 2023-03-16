open Substitution
open Goal
open Types

let var_index = ref 10

let fresh_index () =
  let var = !var_index in
  var_index := var + 1;
  var

let rec eval_step ~global_variable ~need_log :
    environment -> computation -> stream =
 fun env ->
  let eval_step z = eval_step ~global_variable ~need_log env z in
  function
  (* [Nil] | [Cons] | [Thunk] *)
  | Stream x -> x
  | Goal (Unify (t1, t2), (subst, n)) -> (
      if need_log then
        Format.printf "unify %a and %a\n" print_term (apply_subst subst t1)
          print_term (apply_subst subst t2);
      match unify t1 t2 subst with
      (* [UnifyFail] *)
      | None -> nil
      (* [UnifySuccess] *)
      | Some s -> single (s, n))
  (* [Fresh] *)
  | Goal (Fresh (vars, g), (subst, n)) ->
      let amount = List.length vars in
      let new_goal =
        List.fold_right2 subst_in_goal vars
          (List.init amount (fun i ->
               Sem_var (if global_variable then fresh_index () else n + i + 1)))
          g
      in
      thunk @@ goal new_goal (subst, n + amount)
  (* [Disj] *)
  | Goal (Disj (g1, g2), state) ->
      thunk @@ mplus (goal g1 state) @@ stream @@ thunk @@ goal g2 state
  (* [Conj] *)
  | Goal (Conj (g1, g2), state) -> eval_step @@ bind (goal g1 state) g2
  (* [Invoke] *)
  | Goal (Invoke (name, args), state) ->
      eval_step @@ goal (apply name args env) state
  | Force c -> (
      match eval_step c with
      (* [ForceThunk] *)
      | Thunk c -> eval_step c
      (* [ForceNoThunk]*)
      | s -> s)
  | Mplus (c1, c2) -> (
      let s1 = eval_step c1 in
      let s2 = eval_step c2 in
      match s1 with
      (* [MplusNil] *)
      | Nil -> eval_step @@ force @@ stream s2
      (* [MplusCons] *)
      | Cons (state, tl) ->
          cons state @@ thunk @@ mplus (force @@ stream s2) @@ stream tl
      (* [MplusThunk] *)
      | Thunk _ -> thunk @@ mplus (force @@ stream s2) @@ stream s1)
  | Bind (c, g) -> (
      let s = eval_step c in
      match s with
      (* [BindNil] *)
      | Nil -> nil
      (* [BindCons] *)
      | Cons (state, tl) ->
          eval_step
          @@ mplus (goal g state)
          @@ stream @@ thunk
          @@ bind (force @@ stream tl) g
      (* [BindThunk] *)
      | Thunk s -> thunk @@ bind s g)

let rec eval ~global_variable ~need_log :
    environment -> int -> computation -> trace =
 fun env n comp ->
  match (n, eval_step ~need_log ~global_variable env comp) with
  | 0, _ | _, Nil -> [ (comp, None) ]
  | _, Cons (x, xs) ->
      if need_log then
        Format.printf "q=%a\n" print_term
        @@ apply_subst (fst x)
        @@ List.assoc 10 @@ fst x;
      (comp, Some x) :: eval ~need_log ~global_variable env (n - 1) (Stream xs)
  | _, Thunk c -> (comp, None) :: eval ~need_log ~global_variable env (n - 1) c

let run ?(global_variable = false) ?(need_log = false) :
    environment -> int -> goal -> trace =
 fun env c g -> eval ~need_log ~global_variable env c @@ init_comp g
