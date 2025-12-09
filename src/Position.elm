module Position exposing (..)


type alias Position =
    -- Following Mouse Event Position: ( Left Offset, Top Offset )
    ( Float, Float )


diff : Position -> Position -> ( Float, Float )
diff ( b1, b2 ) ( a1, a2 ) =
    ( a1 - b1, a2 - b2 )


add : Position -> ( Float, Float ) -> Position
add ( a1, a2 ) ( d1, d2 ) =
    ( a1 + d1, a2 + d2 )


{-| The first argument here isn't really a position... it's likely the result of `diff`.
Maybe this could be in a new Vector file but this seems okay for now.
-}
magnitude : ( Float, Float ) -> Float
magnitude ( a1, a2 ) =
    sqrt (a1 ^ 2 + a2 ^ 2)
