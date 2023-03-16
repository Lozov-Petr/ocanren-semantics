open Substitution
open Goal

let appendo : relation =
  ( "appendo",
    ( [ "a"; "b"; "ab" ],
      Var "a" === nil &&& (Var "b" === Var "ab")
      ||| fresh [ "h"; "t"; "ab'" ]
            (Var "a" === cons (Var "h") (Var "t")
            &&& (cons (Var "h") (Var "ab'") === Var "ab")
            &&& invoke "appendo" [ Var "t"; Var "b"; Var "ab'" ]) ) )

let list1 = list constant [ "1" ]
let list12 = list constant [ "1"; "2" ]
let list3 = list constant [ "3" ]
let list123 = list constant [ "1"; "2"; "3" ]
let list1234 = list constant [ "1"; "2"; "3"; "4" ]

let reverso : relation =
  ( "reverso",
    ( [ "a"; "b" ],
      Var "a" === nil &&& (Var "b" === nil)
      ||| fresh [ "h"; "t"; "a'" ]
            (Var "a" === cons (Var "h") (Var "t")
            &&& invoke "appendo" [ Var "a'"; cons (Var "h") nil; Var "b" ]
            &&& invoke "reverso" [ Var "t"; Var "a'" ]) ) )
