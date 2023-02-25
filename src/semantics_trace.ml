open Stream

let rec eval_step : environment -> stream -> state option * stream =
 fun env ->
  let eval_step x = eval_step env x in
  function
  (* [Nil] *)
  | Nil -> (None, Nil)
  (* [UnifyFail] and [UnifySuccess] *)
  | Leaf (Unify (t1, t2), (subst, n)) -> (
      match unify t1 t2 subst with
      (* [UnifySuccess] *)
      | Some subst -> (Some (subst, n), Nil)
      (* [UnifyFail] *)
      | None -> (None, Nil))
  (* [Disj] *)
  | Leaf (Disj (g1, g2), state) ->
      (None, Mplus (Leaf (g1, state), Leaf (g2, state)))
  (* [Conj] *)
  | Leaf (Conj (g1, g2), state) -> eval_step @@ Bind (Leaf (g1, state), g2)
  (* [Fresh] *)
  | Leaf (Fresh (vars, g), (subst, n)) ->
      let amount = List.length vars in
      ( None,
        Leaf
          ( List.fold_right2 subst_in_goal vars
              (List.init amount (fun i -> Sem_var (n + i + 1)))
              g,
            (subst, n + amount) ) )
  (* [Invoke] *)
  | Leaf (Invoke (name, args), state) ->
      eval_step @@ Leaf (apply name args env, state)
  (* [MplusNil], [MplusSingle], [MplusCons] and [MplusDelay] *)
  | Mplus (s1, s2) -> (
      match eval_step s1 with
      (* [MplusNil] *)
      | None, Nil -> eval_step s2
      (* [MplusSingle] *)
      | ans, Nil -> (ans, Mplus (s2, Nil))
      (* [MplusDelay] *)
      | None, s1 -> (None, Mplus (s2, s1))
      (* [MplusCons] *)
      | ans, s1 -> (ans, Mplus (s2, s1)))
  (* [BindNil], [BindSingle], [BindCons] and [BindDelay] *)
  | Bind (s, g) -> (
      match eval_step s with
      (* [BindNil] *)
      | None, Nil -> (None, Nil)
      (* [BindSingle] *)
      | Some ans, Nil -> eval_step @@ Mplus (Leaf (g, ans), Bind (Nil, g))
      (* [BindDelay] *)
      | None, s -> (None, Bind (s, g))
      (* [BindCons] *)
      | Some ans, s -> eval_step @@ Mplus (Leaf (g, ans), Bind (s, g)))

let rec eval : environment -> int -> stream -> trace =
 fun env n stream ->
  match (n, stream) with
  | 0, _ | _, Nil -> [ (None, Nil) ]
  | _, stream ->
      let ans, stream' = eval_step env stream in
      (ans, stream) :: eval env (n - 1) stream'
