module Table.View exposing (..)

import Array exposing (Array)
import Card exposing (Card)
import Card.View
import Css exposing (border3, borderRadius, hex, px, solid)
import Deck exposing (Deck)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)
import Position exposing (Position)
import Table exposing (CardLoc(..), Depth, cascadesCount)


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
    (width - toFloat cascadesCount * (Card.View.width + padding)) / 2


cascadesTop : Float
cascadesTop =
    165


pileSpacing : Float
pileSpacing =
    -- pixels between the top of one card and the top of the next card in a pile
    25


pileDepthOffset : Depth -> Float
pileDepthOffset depth =
    toFloat depth * pileSpacing


positionFor : CardLoc -> Position
positionFor cardLoc =
    case cardLoc of
        CascadeLoc column row ->
            let
                left =
                    cascadesOffset + toFloat column * (Card.View.width + padding)

                top =
                    cascadesTop + toFloat row * pileSpacing
            in
            ( left, top )

        Hand _ ->
            ( 0, 0 )


recursiveDeal : Int -> Int -> Array (List Card) -> Deck -> Array (List Card)
recursiveDeal row column cascades deck =
    if column == cascadesCount then
        recursiveDeal (row + 1) 0 cascades deck

    else
        let
            ( topCard, restDeck ) =
                Deck.draw deck
        in
        case topCard of
            Just card ->
                let
                    cardLoc =
                        CascadeLoc column row

                    positionedCard =
                        { card | position = positionFor cardLoc, zIndex = zIndexFor cardLoc }

                    cascade =
                        Array.get column cascades |> Maybe.withDefault []

                    updatedCascades =
                        Array.set column (positionedCard :: cascade) cascades
                in
                recursiveDeal row (column + 1) updatedCascades restDeck

            Nothing ->
                cascades


deal : Deck -> Array (List Card)
deal deck =
    recursiveDeal 0 0 (Array.initialize cascadesCount (always [])) deck


zIndexFor : CardLoc -> Int
zIndexFor cardLoc =
    case cardLoc of
        CascadeLoc _ row ->
            row

        Hand depth ->
            150 - depth
