open Stream

let appendo : relation =
  ( "appendo",
    ( [ "x"; "y"; "xy" ],
      Var "x" === nil &&& (Var "xy" === Var "y")
      ||| fresh [ "e"; "xs"; "xys" ]
            (Var "x" === cons (Var "e") (Var "xs")
            &&& (Var "xy" === cons (Var "e") (Var "xys"))
            &&& invoke "appendo" [ Var "xs"; Var "y"; Var "xys" ]) ) )

let list1 = list constant [ "1"; "2" ]
let list2 = list constant [ "3" ]
let list3 = list constant [ "1"; "2"; "3" ]

let reverso : relation =
  ( "reverso",
    ( [ "x"; "y" ],
      Var "x" === nil &&& (Var "y" === nil)
      ||| fresh [ "e"; "xs"; "ys" ]
            (Var "x" === cons (Var "e") (Var "xs")
            &&& invoke "reverso" [ Var "xs"; Var "ys" ]
            &&& invoke "appendo" [ Var "ys"; cons (Var "e") nil; Var "y" ]) ) )
