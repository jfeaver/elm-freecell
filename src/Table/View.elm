module Table.View exposing
    ( TableLoc(..)
    , backgroundHex
    , cardMark
    , cascadesMargin
    , cascadesTop
    , deal
    , expandedPlayHeight
    , height
    , horizontalOffset
    , locFor
    , padding
    , pileDepthOffset
    , positionFor
    , topOffset
    , width
    , zIndexFor
    )

import Array exposing (Array)
import Basics.Extra exposing (divMod)
import Card exposing (Card, Suit(..))
import Card.View
import Cascade exposing (Column, Row)
import Css exposing (border3, borderRadius, hex, px, solid)
import Deck exposing (Deck)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)
import Maybe.Extra
import Position exposing (Position)
import Table exposing (CardLoc(..), Cell, Depth, Table)
import UI


type TableLoc
    = TableCascade Column Row
    | TableCell Cell
    | TableFoundation Suit


width : Float
width =
    UI.windowWidth


height : Float
height =
    width * 3 / 4 |> UI.roundToWhole


borderWidth : Float
borderWidth =
    3 |> UI.zoomedR


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
    12 |> UI.zoomedR


halfPadding : Float
halfPadding =
    padding / 2 |> UI.roundToHalf


doublePadding : Float
doublePadding =
    padding * 2 |> UI.roundToHalf


topOffset : Float
topOffset =
    40 |> UI.zoomedR


horizontalOffset : Float
horizontalOffset =
    30 |> UI.zoomedR


{-| The left/right margin of the cascades.
-}
cascadesMargin : Table -> Float
cascadesMargin table =
    (width - toFloat table.cascadesCount * (Card.View.width + padding))
        / 2
        |> UI.roundToHalf


cascadesTop : Float
cascadesTop =
    topOffset
        + Card.View.height
        + doublePadding
        |> UI.roundToHalf


{-| pixels between the top of one card and the top of the next card in a pile
-}
pileSpacing : Float
pileSpacing =
    25 |> UI.zoomedR


pileDepthOffset : Depth -> Float
pileDepthOffset depth =
    toFloat depth
        * pileSpacing
        |> UI.roundToHalf


positionFor : Table -> CardLoc -> Position
positionFor table cardLoc =
    case cardLoc of
        CascadeLoc column row ->
            let
                left =
                    cascadesMargin table + toFloat column * (Card.View.width + padding)

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


cascadesLocFor : Table -> Position -> Maybe TableLoc
cascadesLocFor table ( left, top ) =
    let
        xMargin =
            cascadesMargin table

        x =
            left - xMargin

        y =
            top - cascadesTop - 21.5

        ( column, columnX ) =
            divMod (x |> round) (Card.View.width + padding |> round)

        row =
            (y / pileSpacing) |> floor
    in
    if y < 0 || x < 0 || left > (width - xMargin - padding) then
        Nothing

    else if (columnX |> toFloat) > Card.View.width then
        Nothing

    else
        Just <| TableCascade column row


cellsLocFor : Table -> Position -> Maybe TableLoc
cellsLocFor table ( left, top ) =
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
            toFloat table.cellsCount * (Card.View.width + padding)

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


locFor : Table -> Position -> Maybe TableLoc
locFor table position =
    Maybe.Extra.or (cascadesLocFor table position) (cellsLocFor table position)
        |> Maybe.Extra.or (foundationLocFor position)


recursiveDeal : Table -> Row -> Column -> Array (List Card) -> Deck -> Array (List Card)
recursiveDeal table row column cascades deck =
    if column == table.cascadesCount then
        recursiveDeal table (row + 1) 0 cascades deck

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
                        { card | position = positionFor table cardLoc, zIndex = zIndexFor cardLoc }

                    cascade =
                        Array.get column cascades |> Maybe.withDefault []

                    updatedCascades =
                        Array.set column (positionedCard :: cascade) cascades
                in
                recursiveDeal table row (column + 1) updatedCascades restDeck

            Nothing ->
                cascades


deal : Table -> Deck -> Table
deal table deck =
    { table | cascades = recursiveDeal table 0 0 table.cascades deck }


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


{-| Returns number of pixels that play is extending beyond normal table height
-}
expandedPlayHeight : Table -> Float
expandedPlayHeight table =
    let
        doLongestCascade cascade longest =
            max (List.length cascade) longest

        longestCascade =
            Array.foldl doLongestCascade 0 table.cascades

        playExtension =
            cascadesTop + toFloat longestCascade * pileSpacing + doublePadding + Card.View.height - height
    in
    if playExtension > 0 then
        playExtension

    else
        0
