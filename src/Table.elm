module Table exposing
    ( CardLoc(..)
    , Cell
    , Depth
    , Table
    , TableLoc(..)
    , cascadeDepth
    , cascadeEmpty
    , cellEmpty
    , emptyCascades
    , emptyCells
    , getTableCard
    , new
    , pickPile
    , stackTo
    , tableCardLocToCardLoc
    )

import Array exposing (Array)
import Card exposing (Card, Suit(..))
import Card.Rank
import Cascade exposing (Column, Row)
import List.Extra
import Maybe.Extra
import Pile exposing (Pile, validPile)


type alias Foundation =
    Maybe Card


type alias Table =
    { cells : Array (Maybe Card)
    , cellsCount : Int
    , cascades : Array (List Card)
    , cascadesCount : Int
    , diamonds : Foundation
    , clubs : Foundation
    , hearts : Foundation
    , spades : Foundation
    }


type alias Cell =
    Int


type alias Depth =
    Int


type CardLoc
    = CascadeLoc Column Row
    | CellLoc Cell
    | Hand Depth
    | FoundationLoc Suit


type TableLoc
    = TableCascade Column Row
    | TableCell Cell
    | TableFoundation Suit


new : Int -> Int -> Table
new cellsCount cascadesCount =
    { cells = Array.initialize cellsCount (always Nothing)
    , cellsCount = cellsCount
    , cascades = Array.initialize cascadesCount (always [])
    , cascadesCount = cascadesCount
    , diamonds = Nothing
    , clubs = Nothing
    , hearts = Nothing
    , spades = Nothing
    }


stackHeight : List Card -> Row -> Int
stackHeight cards row =
    let
        columnDepth =
            List.length cards
    in
    columnDepth - row


{-| Creates two stacks from the card's cascade and returns them as two lists in a tuple.
-| The first stack is the card specified and all cards below it. The second stack is the
-| rest of the cards above the specified card in the cascade.
-}
stackTo : ( Column, Row ) -> Table -> ( List Card, List Card )
stackTo ( column, row ) table =
    table.cascades
        |> Array.get column
        |> Maybe.withDefault []
        |> (\cards -> List.Extra.partitionN (stackHeight cards row) cards)


cascadeDepth : Column -> Table -> Maybe Int
cascadeDepth column table =
    table.cascades
        |> Array.get column
        |> Maybe.map List.length


pickPile : CardLoc -> Table -> Maybe ( Pile, Table )
pickPile cardLoc table =
    case cardLoc of
        CascadeLoc column row ->
            let
                validatePileMapper ( pile, leftBehind ) =
                    if validPile pile then
                        Just ( pile, leftBehind )

                    else
                        Nothing

                -- Generates data like: Maybe (pile, leftBehind)
                mDividedPiles : Maybe ( Pile, List Card )
                mDividedPiles =
                    stackTo ( column, row ) table
                        |> validatePileMapper
            in
            mDividedPiles
                |> Maybe.map
                    (\( pile, leftBehind ) ->
                        ( pile, { table | cascades = Array.set column leftBehind table.cascades } )
                    )

        Hand _ ->
            Nothing

        CellLoc cell ->
            let
                hasCardMapper card =
                    ( [ card ], { table | cells = Array.set cell Nothing table.cells } )
            in
            Maybe.map hasCardMapper (getCell cell table)

        FoundationLoc suit ->
            case suit of
                Diamonds ->
                    table.diamonds
                        |> Maybe.map (\card -> ( [ card ], { table | diamonds = Card.Rank.decrementCard card } ))

                Clubs ->
                    table.clubs
                        |> Maybe.map (\card -> ( [ card ], { table | clubs = Card.Rank.decrementCard card } ))

                Hearts ->
                    table.hearts
                        |> Maybe.map (\card -> ( [ card ], { table | hearts = Card.Rank.decrementCard card } ))

                Spades ->
                    table.spades
                        |> Maybe.map (\card -> ( [ card ], { table | spades = Card.Rank.decrementCard card } ))


getCell : Cell -> Table -> Maybe Card
getCell cell { cells } =
    Array.get cell cells
        |> Maybe.Extra.dig


tableCardLocToCardLoc : TableLoc -> CardLoc
tableCardLocToCardLoc tableLoc =
    case tableLoc of
        TableCascade column row ->
            CascadeLoc column row

        TableCell cell ->
            CellLoc cell

        TableFoundation foundation ->
            FoundationLoc foundation


getTableCard : TableLoc -> Table -> Maybe Card
getTableCard tableLoc table =
    let
        getStackCard : Row -> List Card -> Maybe Card
        getStackCard row stack =
            if row == 0 then
                List.head stack

            else
                List.tail stack
                    |> Maybe.andThen (\rest -> getStackCard (row - 1) rest)
    in
    case tableLoc of
        TableCascade column row ->
            table.cascades
                |> Array.get column
                |> Maybe.andThen (getStackCard row)

        TableCell cell ->
            getCell cell table

        TableFoundation foundation ->
            case foundation of
                Diamonds ->
                    table.diamonds

                Clubs ->
                    table.clubs

                Hearts ->
                    table.hearts

                Spades ->
                    table.spades


cellEmpty : Cell -> Table -> Bool
cellEmpty cell table =
    getCell cell table
        |> Maybe.Extra.isNothing


cascadeEmpty : Column -> Table -> Bool
cascadeEmpty column table =
    table.cascades
        |> Array.get column
        |> Maybe.withDefault []
        |> List.length
        |> (==) 0


countEmptyCascades : Column -> Int -> Table -> Int
countEmptyCascades column count table =
    if column == table.cascadesCount then
        count

    else
        let
            newCount =
                if cascadeEmpty column table then
                    count + 1

                else
                    count
        in
        -- TODO: Remove recursion?
        countEmptyCascades (column + 1) newCount table


emptyCascades : Table -> Int
emptyCascades =
    countEmptyCascades 0 0


countEmptyCells : Cell -> Int -> Table -> Int
countEmptyCells cell count table =
    if cell == table.cellsCount then
        count

    else
        let
            mCard =
                Array.get cell table.cells
                    |> Maybe.Extra.dig

            newCount =
                case mCard of
                    Just _ ->
                        count

                    Nothing ->
                        count + 1
        in
        countEmptyCells (cell + 1) newCount table


emptyCells : Table -> Int
emptyCells =
    countEmptyCells 0 0
