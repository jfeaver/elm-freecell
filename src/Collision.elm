module Collision exposing (between)

import Hitbox exposing (Hitbox)


between : Hitbox -> Hitbox -> Bool
between ( ( x1a, y1a ), ( x2a, y2a ) ) ( ( x1b, y1b ), ( x2b, y2b ) ) =
    let
        overlaps =
            [ x2b - x1a
            , x2a - x1b
            , y2a - y1b
            , y2b - y1a
            ]
    in
    List.all ((<) 0) overlaps
