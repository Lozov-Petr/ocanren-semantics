open Stream

let rec eval_step env =
  let eval_step x = eval_step env x in

  let mplus xs ys =
    match eval_step xs with
    | None, Nil -> eval_step ys
    | (Some _ as ans), xs -> (ans, Mplus (ys, xs))
    | None, xs -> (None, Mplus (ys, xs))
  in
  let bind s f =
    match eval_step s with
    | None, Nil -> (None, Nil)
    | Some ans, s -> eval_step @@ Mplus (Leaf (f, ans), Bind (s, f))
    | None, s -> (None, Bind (s, f))
  in

  let leaf g state =
    let unify t1 t2 (subst, n) =
      match unify t1 t2 subst with
      | Some subst -> (Some (subst, n), Nil)
      | None -> (None, Nil)
    in
    let disj g1 g2 st = (None, Mplus (Leaf (g1, st), Leaf (g2, st))) in
    let conj g1 g2 st = bind (Leaf (g1, st)) g2 in
    let fresh vs g (subst, n) =
      let amount = List.length vs in
      ( None,
        Leaf
          ( List.fold_right2 subst_in_goal vs
              (List.init amount (fun i -> Sem_var (n + i + 1)))
              g,
            (subst, n + amount) ) )
    in
    let invoke name args st = eval_step @@ Leaf (apply name args env, st) in
    match g with
    | Unify (t1, t2) -> unify t1 t2 state
    | Disj (g1, g2) -> disj g1 g2 state
    | Conj (g1, g2) -> conj g1 g2 state
    | Fresh (vars, g) -> fresh vars g state
    | Invoke (name, args) -> invoke name args state
  in
  function
  | Nil -> (None, Nil)
  | Mplus (s1, s2) -> mplus s1 s2
  | Bind (s, g) -> bind s g
  | Leaf (g, st) -> leaf g st

let rec eval : environment -> int -> stream -> trace =
 fun env n stream ->
  match (n, stream) with
  | 0, _ | _, Nil -> [ (None, Nil) ]
  | _, stream ->
      let ans, stream' = eval_step env stream in
      (ans, stream) :: eval env (n - 1) stream'
