open Stream
open OCanren
open Semantics

let compare_traces env goal =
  let init_stream = init_stream goal in
  Semantics_trace.eval env (-1) init_stream
  = Ocanren_trace.eval env (-1) init_stream

let%expect_test "" =
  let open Examples in
  Printf.printf "%B" @@ compare_traces [ appendo ] @@ fresh [ "q" ]
  @@ invoke "appendo" [ list1; list2; Var "q" ];
  [%expect {| true |}]

let%expect_test "" =
  let open Examples in
  Printf.printf "%B" @@ compare_traces [ appendo ]
  @@ fresh [ "q"; "p" ]
  @@ invoke "appendo" [ Var "q"; Var "p"; list3 ];
  [%expect {| true |}]

let%expect_test "" =
  let open Examples in
  Printf.printf "%B"
  @@ compare_traces [ reverso; appendo ]
  @@ fresh [ "q" ]
  @@ invoke "reverso" [ list1; Var "q" ];
  [%expect {| true |}]

let%expect_test "" =
  let open Examples in
  Printf.printf "%B"
  @@ compare_traces [ reverso; appendo ]
  @@ fresh [ "q" ]
  @@ invoke "reverso" [ list3; Var "q" ];
  [%expect {| true |}]
