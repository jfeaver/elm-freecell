module Cascade exposing (..)

{-| In FreeCell, cascades are the starting stacks of disordered cards (though they become ordered as you progress).
-}


{-| Columns start at 0 and increment from left to right.
-}
type alias Column =
    Int


{-| Rows start at 0 from the back of the stack (top of the screen) and increment through the stack.
-}
type alias Row =
    Int
