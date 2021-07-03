module Table.View exposing (..)

import Array exposing (Array)
import Card exposing (Card, Suit(..))
import Card.View
import Css exposing (border3, borderRadius, hex, px, solid)
import Deck exposing (Deck)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)
import Maybe.Extra
import Position exposing (Position)
import Table exposing (CardLoc(..), Cell, Column, Depth, Row, cascadesCount, cellsCount)


type TableLoc
    = TableCascade Column
    | TableCell Cell
    | TableFoundation Suit


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


halfPadding : Float
halfPadding =
    padding / 2


topOffset : Float
topOffset =
    40


horizontalOffset : Float
horizontalOffset =
    30


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

        CellLoc cell ->
            ( horizontalOffset + toFloat cell * (Card.View.width + padding), topOffset )

        FoundationLoc suit ->
            let
                invertedSuitIndex =
                    4 - Card.suitIndex suit
            in
            ( width - (horizontalOffset + toFloat invertedSuitIndex * Card.View.width + toFloat (invertedSuitIndex - 1) * padding), topOffset )


cascadesLocFor : Position -> Maybe TableLoc
cascadesLocFor ( left, top ) =
    let
        leftCascadesOffset =
            cascadesOffset - halfPadding

        topCascadesOffset =
            cascadesTop - halfPadding

        leftCascades =
            left - leftCascadesOffset

        topCascades =
            top - topCascadesOffset
    in
    if topCascades < 0 || leftCascades < 0 || left > (width - leftCascadesOffset) then
        Nothing

    else
        Just <| TableCascade (floor (leftCascades / (Card.View.width + padding)))


cellsLocFor : Position -> Maybe TableLoc
cellsLocFor ( left, top ) =
    let
        leftCellsOffset =
            horizontalOffset - halfPadding

        topCellsOffset =
            topOffset - halfPadding

        leftCells =
            left - leftCellsOffset

        topCells =
            top - topCellsOffset

        maxLeft =
            toFloat cellsCount * (Card.View.width + padding)

        maxTop =
            Card.View.height + padding
    in
    if leftCells < 0 || leftCells > maxLeft || topCells < 0 || topCells > maxTop then
        Nothing

    else
        Just <| TableCell (floor (leftCells / (Card.View.width + padding)))


foundationLocFor : Position -> Maybe TableLoc
foundationLocFor ( left, top ) =
    let
        rightFoundationsOffset =
            width - horizontalOffset + halfPadding

        leftFoundationsOffset =
            rightFoundationsOffset - 4 * cardBoxWidth

        cardBoxWidth =
            Card.View.width + padding

        topFoundationsOffset =
            topOffset - halfPadding

        leftFoundations =
            left - leftFoundationsOffset

        topFoundations =
            top - topFoundationsOffset

        maxTop =
            Card.View.height + padding
    in
    if leftFoundations > 0 && leftFoundations < cardBoxWidth && topFoundations > 0 && topFoundations < maxTop then
        Just (TableFoundation Diamonds)

    else if leftFoundations > cardBoxWidth && leftFoundations < 2 * cardBoxWidth && topFoundations > 0 && topFoundations < maxTop then
        Just (TableFoundation Clubs)

    else if leftFoundations > 2 * cardBoxWidth && leftFoundations < 3 * cardBoxWidth && topFoundations > 0 && topFoundations < maxTop then
        Just (TableFoundation Hearts)

    else if leftFoundations > 3 * cardBoxWidth && leftFoundations < 4 * cardBoxWidth && topFoundations > 0 && topFoundations < maxTop then
        Just (TableFoundation Spades)

    else
        Nothing


locFor : Position -> Maybe TableLoc
locFor position =
    Maybe.Extra.or (cascadesLocFor position) (cellsLocFor position)
        |> Maybe.Extra.or (foundationLocFor position)


recursiveDeal : Row -> Column -> Array (List Card) -> Deck -> Array (List Card)
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

        CellLoc _ ->
            1

        FoundationLoc _ ->
            -- Top card is two, decrement card is one
            2
