module Basics.Extra exposing (..)

{-| Returns the modulo and the remainder:

> divMod 8 3
> (2,2)
> divMod 9 3
> (3,0)
> divMod 9 4
> (2,1)
> divMod 9 2
> (4,1)

-}


divMod : Int -> Int -> ( Int, Int )
divMod a b =
    let
        div =
            a // b
    in
    ( div, a - b * div )
