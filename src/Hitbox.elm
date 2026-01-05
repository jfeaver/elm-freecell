module Hitbox exposing
    ( Hitbox
    , center
    , fromDelta
    , height
    , width
    , x1
    , x2
    , y1
    , y2
    )

import Position exposing (Position)


{-| literally a box

( northwest point, southeast point )
( smaller values point, larger values point )

-}
type alias Hitbox =
    ( Position, Position )


fromDelta : Position -> Position -> Hitbox
fromDelta position delta =
    ( position, Position.add position delta )


width : Hitbox -> Float
width hitbox =
    x2 hitbox - x1 hitbox


height : Hitbox -> Float
height hitbox =
    y2 hitbox - y1 hitbox


center : Hitbox -> Position
center ( ( hx1, hy1 ), ( hx2, hy2 ) ) =
    let
        x =
            ((hx2 - hx1) / 2) + hx1

        y =
            ((hy2 - hy1) / 2) + hy1
    in
    ( x, y )


x1 : Hitbox -> Float
x1 =
    Tuple.first >> Tuple.first


x2 : Hitbox -> Float
x2 =
    Tuple.second >> Tuple.first


y1 : Hitbox -> Float
y1 =
    Tuple.first >> Tuple.second


y2 : Hitbox -> Float
y2 =
    Tuple.second >> Tuple.second
