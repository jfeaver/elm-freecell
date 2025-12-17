module Modal exposing (..)

import Css exposing (pct, px)
import Html.Styled as Html exposing (Html, div)
import Html.Styled.Attributes exposing (css)


view : List (Html.Attribute msg) -> List (Html msg) -> Html msg
view attributes children =
    div attributes
        [ div
            [ css
                [ Css.displayFlex
                , Css.justifyContent Css.center
                , Css.alignItems Css.center
                , Css.position Css.fixed
                , Css.top (px 0)
                , Css.left (px 0)
                , Css.width (pct 100)
                , Css.height (pct 100)
                , Css.zIndex (Css.int 1000)
                ]
            ]
            children
        , div
            [ css
                [ Css.position Css.fixed
                , Css.height (pct 100)
                , Css.top (px 0)
                , Css.left (px 0)
                , Css.width (pct 100)
                , Css.zIndex (Css.int 999)
                , Css.backgroundColor (Css.rgba 0 0 0 0.4)
                ]
            ]
            []
        ]
