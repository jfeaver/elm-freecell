module UI exposing (..)


zoom : Float
zoom =
    1.3


zoomed : Float -> Float
zoomed n =
    n * zoom


zoomedR : Float -> Float
zoomedR =
    zoomed >> roundToHalf


windowWidth : Float
windowWidth =
    800 * zoom |> roundToWhole


roundToHalf : Float -> Float
roundToHalf n =
    (n * 2 |> round |> toFloat) / 2


roundToWhole : Float -> Float
roundToWhole =
    round >> toFloat
