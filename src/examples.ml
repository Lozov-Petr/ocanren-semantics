open Substitution
open Goal

let invoke_appendo a b ab = invoke "appendo" [ a; b; ab ]

let appendo : relation =
  ( "appendo",
    ( [ "a"; "b"; "ab" ],
      let a, b, ab = (Var "a", Var "b", Var "ab") in
      a === nil &&& (b === ab)
      ||| fresh [ "h"; "t"; "ab'" ]
            (let h, t, ab' = (Var "h", Var "t", Var "ab'") in
             a === cons h t &&& (cons h ab' === ab) &&& invoke_appendo t b ab')
    ) )

let invoke_reverso a b = invoke "reverso" [ a; b ]

let reverso : relation =
  ( "reverso",
    ( [ "a"; "b" ],
      let a, b = (Var "a", Var "b") in
      a === nil &&& (b === nil)
      ||| fresh [ "h"; "t"; "a'" ]
            (let h, t, a' = (Var "h", Var "t", Var "a'") in
             a === cons h t
             &&& invoke_appendo a' (cons h nil) b
             &&& invoke_reverso t a') ) )

let list1 = list constant [ "1" ]
let list12 = list constant [ "1"; "2" ]
let list3 = list constant [ "3" ]
let list123 = list constant [ "1"; "2"; "3" ]
let list1234 = list constant [ "1"; "2"; "3"; "4" ]
