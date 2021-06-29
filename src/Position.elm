module Position exposing (..)


type alias Position =
    -- Following Mouse Event Position: ( Left Offset, Top Offset )
    ( Float, Float )


diff : Position -> Position -> ( Float, Float )
diff ( a1, a2 ) ( b1, b2 ) =
    ( a1 - b1, a2 - b2 )


add : Position -> ( Float, Float ) -> Position
add ( a1, a2 ) ( d1, d2 ) =
    ( a1 + d1, a2 + d2 )
