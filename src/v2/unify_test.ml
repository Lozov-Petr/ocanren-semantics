open Substitution
open Eval
open Goal
open Examples

let __ _ =
  Format.printf "appendo [1; 2] [3] q\n";
  ignore
  @@ (Eval_trace.run ~global_variable:true ~need_log:true)
       [ appendo ] (-1)
       (fresh [ "q" ]
       @@
       let q = Var "q" in
       invoke_appendo list12 list3 q)

let __ _ =
  Format.printf "reverso q [1; 2]\n";
  ignore
  @@ (Eval_trace.run ~global_variable:true ~need_log:true)
       [ appendo; reverso ] (-1)
       (fresh [ "q" ]
       @@
       let q = Var "q" in
       invoke_reverso q list12)

let __ _ =
  Format.printf "appendo p q [1; 2; 3; 4] q\n";
  ignore
  @@ (Eval_trace.run ~global_variable:true ~need_log:true)
       [ appendo ] (-1)
       (fresh [ "q"; "p" ]
       @@
       let q, p = (Var "q", Var "p") in
       invoke_appendo q p list1234)

let __ _ =
  Format.printf "reverso q [1]\n";
  ignore
  @@ (Eval_trace.run ~global_variable:true ~need_log:true)
       [ appendo; reverso ] (-1)
       (fresh [ "q" ]
       @@
       let q = Var "q" in
       invoke_reverso q list1)

let __ _ =
  Format.printf "reverso q [1; 2]\n";
  ignore
  @@ (Eval_trace.run ~global_variable:true ~need_log:true)
       [ appendo; reverso ] (-1)
       (fresh [ "q" ]
       @@
       let q = Var "q" in
       invoke_reverso q list12)

let _ =
  Format.printf "reverso [1; 2] q\n";
  ignore
  @@ (Eval_trace.run ~global_variable:true ~need_log:true)
       [ appendo; reverso ] 21
       (fresh [ "q" ]
       @@
       let q = Var "q" in
       invoke_reverso list12 q)
