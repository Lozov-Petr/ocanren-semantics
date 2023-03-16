open Eval
open Goal
open Examples

let __ _ =
  Format.printf "appendo [1; 2] [3] q\n";
  ignore
  @@ (Eval_trace.run ~global_variable:true ~need_log:true)
       [ appendo ] (-1)
       (fresh [ "q" ] @@ invoke "appendo" [ list12; list3; Var "q" ])

let __ _ =
  Format.printf "reverso q [1; 2]\n";
  ignore
  @@ (Eval_trace.run ~global_variable:true ~need_log:true)
       [ appendo; reverso ] (-1)
       (fresh [ "q" ] @@ invoke "reverso" [ Var "q"; list12 ])

let __ _ =
  Format.printf "appendo p q [1; 2; 3; 4] q\n";
  ignore
  @@ (Eval_trace.run ~global_variable:true ~need_log:true)
       [ appendo ] (-1)
       (fresh [ "q"; "p" ] @@ invoke "appendo" [ Var "q"; Var "p"; list1234 ])

let __ _ =
  Format.printf "reverso q [1]\n";
  ignore
  @@ (Eval_trace.run ~global_variable:true ~need_log:true)
       [ appendo; reverso ] (-1)
       (fresh [ "q" ] @@ invoke "reverso" [ Var "q"; list1 ])

let _ =
  Format.printf "reverso q [1; 2]\n";
  ignore
  @@ (Eval_trace.run ~global_variable:true ~need_log:true)
       [ appendo; reverso ] (-1)
       (fresh [ "q" ] @@ invoke "reverso" [ Var "q"; list12 ])
