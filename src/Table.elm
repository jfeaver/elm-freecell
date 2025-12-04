module Table exposing
    ( CardLoc(..)
    , Cell
    , Column
    , Depth
    , Row
    , Table
    , cascadeEmpty
    , cellEmpty
    , emptyCascades
    , emptyCells
    , new
    , pickPile
    , validPile
    )

import Array exposing (Array)
import Card exposing (Card, Suit(..))
import Card.Color
import Card.Rank
import List.Extra
import Maybe.Extra


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


type alias Column =
    Int


type alias Row =
    Int


type alias Cell =
    Int


type alias Depth =
    Int


type CardLoc
    = CascadeLoc Column Row
    | CellLoc Cell
    | Hand Depth
    | FoundationLoc Suit


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


validPilePair : Card -> Card -> Bool
validPilePair top second =
    Card.Rank.increment top.rank == second.rank && Card.Color.notColor (Card.Color.fromCard top) == Card.Color.fromCard second


validPile : List Card -> Bool
validPile cards =
    case cards of
        top :: second :: others ->
            validPilePair top second && validPile (second :: others)

        _ ->
            True


pileHeight : List Card -> Row -> Int
pileHeight cards row =
    let
        columnDepth =
            List.length cards
    in
    columnDepth - row


pickPile : CardLoc -> Table -> Maybe ( List Card, Table )
pickPile cardLoc table =
    case cardLoc of
        CascadeLoc column row ->
            let
                takeTopN columnCards =
                    List.Extra.partitionN (pileHeight columnCards row) columnCards

                validatePileMapper ( pile, leftBehind ) =
                    if validPile pile then
                        Just ( pile, leftBehind )

                    else
                        Nothing

                mDividedPiles : Maybe ( List Card, List Card )
                mDividedPiles =
                    -- Maybe (pile, leftBehind)
                    -- TODO : Cascades should be its own type so that I don't have to add the default always
                    table.cascades
                        |> Array.get column
                        |> Maybe.withDefault []
                        |> takeTopN
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
