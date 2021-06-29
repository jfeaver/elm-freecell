module Table.View exposing (..)

import Card.View
import Css exposing (border3, borderRadius, hex, px, solid)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)


width : Float
width =
    800


height : Float
height =
    600


borderWidth : Float
borderWidth =
    3


backgroundHex : String
backgroundHex =
    "076324"


borderHex : String
borderHex =
    -- darkened background color (20%)
    "054f1c"


cardMark : Attribute msg
cardMark =
    css
        [ border3 (px borderWidth) solid (hex borderHex)
        , borderRadius (px borderWidth)
        , Css.height (px (Card.View.height - 2 * borderWidth))
        , Css.width (px (Card.View.width - 2 * borderWidth))
        ]


padding : Float
padding =
    12


cascadesOffset : Float
cascadesOffset =
    (width - 8 * (Card.View.width + padding)) / 2


cascadesTop : Float
cascadesTop =
    165


pileSpacing : Float
pileSpacing =
    -- pixels between the top of one card and the top of the next card in a pile
    25
