module UI exposing (..)

import Css


zoom : Float
zoom =
    1.3


zoomed : Float -> Float
zoomed n =
    n * zoom


indicatorWidth : Float
indicatorWidth =
    5.0 |> zoomedR


cardHighlightWidth : Float
cardHighlightWidth =
    3.0 |> zoomedR


indicatorRadius : Float
indicatorRadius =
    3.8 |> zoomedR


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


pileIndicatorColor : Css.Color
pileIndicatorColor =
    Css.rgb 161 255 255


pickablePileIndicatorColor : Css.Color
pickablePileIndicatorColor =
    Css.rgb 184 255 51


unpickablePileIndicatorColor : Css.Color
unpickablePileIndicatorColor =
    Css.rgb 255 155 44


cardHighlightColor : Css.Color
cardHighlightColor =
    Css.rgb 227 218 156
