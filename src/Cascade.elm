module Cascade exposing (..)

{-| In FreeCell, cascades are the starting stacks of disordered cards (though they become ordered as you progress).
-}


{-| Columns start at 0 and increment from left to right.
-}
type alias Column =
    Int


{-| Rows start at 0 from the back of the stack (top of the screen) and increment through the stack.
Note that the lists of cards held by Table.cascades are inverted (cards in the zeroth row are last in the list).
-}
type alias Row =
    Int
