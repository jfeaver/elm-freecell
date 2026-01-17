module Table.View exposing
    ( backgroundHex
    , cardMark
    , cascadesMargin
    , cascadesTop
    , deal
    , expandedPlayHeight
    , height
    , horizontalOffset
    , locFor
    , locsForHitbox
    , newTableWithCachedHitboxes
    , padding
    , positionFor
    , stackOffset
    , stackSpacing
    , topOffset
    , width
    , zIndexFor
    )

import Array exposing (Array)
import Basics.Extra exposing (divMod)
import Card exposing (Card, Suit(..))
import Card.Rank
import Card.View
import Cascade exposing (Column, Row)
import Collision
import Css exposing (border3, borderRadius, hex, px, solid)
import Deck exposing (Deck)
import Hitbox exposing (Hitbox)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)
import Maybe.Extra
import Position exposing (Position)
import Table exposing (CardLoc(..), Depth, Table, TableLoc(..))
import UI


newTableWithCachedHitboxes : Int -> Int -> Table
newTableWithCachedHitboxes cellsCount cascadesCount =
    Table.new cellsCount cascadesCount
        |> (\t -> { t | hitboxes = newHitboxQuadMap t })


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
cascadesMargin : Int -> Float
cascadesMargin cascadesCount =
    (width - toFloat cascadesCount * (Card.View.width + padding))
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
stackSpacing : Float
stackSpacing =
    25 |> UI.zoomedR


stackOffset : Depth -> Float
stackOffset depth =
    toFloat depth
        * stackSpacing
        |> UI.roundToHalf


positionFor : Table -> TableLoc -> Position
positionFor table tableLoc =
    case tableLoc of
        CascadeLoc column row ->
            let
                left =
                    cascadesMargin table.cascadesCount + toFloat column * (Card.View.width + padding)

                top =
                    cascadesTop + toFloat row * stackSpacing
            in
            ( left, top )

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
            cascadesMargin table.cascadesCount

        x =
            left - xMargin

        y =
            top - cascadesTop

        ( column, columnX ) =
            divMod (x |> round) (Card.View.width + padding |> round)

        maxPointerRow =
            (y / stackSpacing) |> floor

        cascadeMaxRow =
            Table.cascadeDepth column table
                |> Maybe.map (\depth -> depth - 1)
                |> Maybe.withDefault maxPointerRow

        row =
            min maxPointerRow cascadeMaxRow

        bottomOfLastCard =
            ((row |> toFloat) * stackSpacing) + Card.View.height
    in
    if y < 0 || y > bottomOfLastCard || x < 0 || left > (width - xMargin - padding) then
        Nothing

    else if (columnX |> toFloat) > Card.View.width then
        Nothing

    else
        Just <| CascadeLoc column row


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
        Just <| CellLoc (floor (leftCells / (Card.View.width + padding)))


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
        Just (FoundationLoc Diamonds)

    else if leftFoundations > cardBoxWidth && leftFoundations < 2 * cardBoxWidth && topFoundations > 0 && topFoundations < maxTop then
        Just (FoundationLoc Clubs)

    else if leftFoundations > 2 * cardBoxWidth && leftFoundations < 3 * cardBoxWidth && topFoundations > 0 && topFoundations < maxTop then
        Just (FoundationLoc Hearts)

    else if leftFoundations > 3 * cardBoxWidth && leftFoundations < 4 * cardBoxWidth && topFoundations > 0 && topFoundations < maxTop then
        Just (FoundationLoc Spades)

    else
        Nothing


locFor : Table -> Position -> Maybe TableLoc
locFor table tablePosition =
    Maybe.Extra.or (cascadesLocFor table tablePosition) (cellsLocFor table tablePosition)
        |> Maybe.Extra.or (foundationLocFor tablePosition)


newHitboxQuadMap : Table -> Table.HitboxQuadMap
newHitboxQuadMap table =
    let
        getPos =
            positionFor table

        singleCardHitbox position =
            Hitbox.fromDelta position ( Card.View.width, Card.View.height )

        foundationHitboxes =
            [ FoundationLoc Diamonds, FoundationLoc Clubs, FoundationLoc Hearts, FoundationLoc Spades ]
                |> List.map (\tableLoc -> ( getPos tableLoc |> singleCardHitbox, tableLoc ))

        cellsHitboxes =
            table.cellsCount
                - 1
                |> List.range 0
                |> List.map (CellLoc >> (\tableLoc -> ( getPos tableLoc |> singleCardHitbox, tableLoc )))

        cascadeHitboxHeight =
            stackOffset 20 + Card.View.height

        cascadeHitbox column =
            let
                loc =
                    CascadeLoc column 0

                top =
                    getPos loc
            in
            ( Hitbox.fromDelta top ( Card.View.width, cascadeHitboxHeight ), loc )

        halfCount =
            toFloat table.cascadesCount / 2 |> floor

        cascadesHalf =
            halfCount
                |> List.range 0
                |> List.map cascadeHitbox

        cascadesOthers =
            -- The other half with an overlap of one
            table.cascadesCount
                - 1
                |> List.range (halfCount - 1)
                |> List.map cascadeHitbox
    in
    { foundations = foundationHitboxes
    , cells = cellsHitboxes
    , cascadesHalf = cascadesHalf
    , cascadesOthers = cascadesOthers
    }


type alias HitboxesGetter =
    Table.HitboxQuadMap -> List ( Hitbox, TableLoc )


hitboxesToCheck : Table -> Hitbox -> List HitboxesGetter
hitboxesToCheck table ( ( x1, y1 ), ( x2, y2 ) ) =
    let
        ( foundationX, foundationY ) =
            positionFor table (FoundationLoc Diamonds)

        checkFoundations =
            if x2 >= foundationX && y1 <= foundationY + Card.View.height then
                Just .foundations

            else
                Nothing

        ( cellsX, cellsY ) =
            positionFor table (CellLoc (table.cellsCount - 1))

        checkCells =
            if x1 <= cellsX + Card.View.width && y1 <= cellsY + Card.View.height then
                Just .cells

            else
                Nothing

        halfCount =
            toFloat table.cascadesCount / 2 |> floor

        ( cascadesHalfX, cascadesHalfY ) =
            positionFor table (CascadeLoc (halfCount - 1) 0)

        checkCascadesHalf =
            if x1 <= cascadesHalfX && y2 >= cascadesHalfY then
                Just .cascadesHalf

            else
                Nothing

        checkCascadesOthers =
            if x1 > cascadesHalfX && y2 >= cascadesHalfY then
                Just .cascadesOthers

            else
                Nothing

        all =
            [ checkFoundations, checkCells, checkCascadesHalf, checkCascadesOthers ]

        foldFn : Maybe HitboxesGetter -> List HitboxesGetter -> List HitboxesGetter
        foldFn mGetter soFar =
            case mGetter of
                Just getter ->
                    getter :: soFar

                Nothing ->
                    soFar
    in
    List.foldl foldFn [] all


locsForHitbox : Table -> Hitbox -> List TableLoc
locsForHitbox table incomingHitbox =
    let
        possibleCollisions =
            incomingHitbox
                |> hitboxesToCheck table
                |> List.foldl (\getter hitboxes -> getter table.hitboxes ++ hitboxes) []

        isCollision =
            Collision.between incomingHitbox

        collisionFolder collider collisions =
            if isCollision (collider |> Tuple.first) then
                (collider |> Tuple.second) :: collisions

            else
                collisions
    in
    possibleCollisions
        |> List.foldl collisionFolder []


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
                    tableLoc =
                        CascadeLoc column row

                    cardLoc =
                        StaticLoc (CascadeLoc column row)

                    positionedCard =
                        { card | position = positionFor table tableLoc, zIndex = zIndexFor ( cardLoc, card ) }

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


zIndexFor : ( CardLoc, Card ) -> Int
zIndexFor ( cardLoc, card ) =
    case cardLoc of
        -- Animating on the table gets a bump of 65 (in Main.elm)
        StaticLoc (CascadeLoc _ row) ->
            row

        Hand depth ->
            150 - depth

        StaticLoc (CellLoc _) ->
            1

        StaticLoc (FoundationLoc _) ->
            card.rank
                |> Card.Rank.toValue


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
            cascadesTop + toFloat longestCascade * stackSpacing + doublePadding + Card.View.height - height
    in
    if playExtension > 0 then
        playExtension

    else
        0
