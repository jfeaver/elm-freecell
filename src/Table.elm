module Table exposing
    ( CardLoc(..)
    , Cell
    , Column
    , Depth
    , Row
    , Table
    , cascadesCount
    , cellEmpty
    , cellsCount
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
    , cascades : Array (List Card)
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


cascadesCount : Int
cascadesCount =
    8


cellsCount : Int
cellsCount =
    4


new : Table
new =
    { cells = Array.initialize cellsCount (always Nothing)
    , cascades = Array.empty
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


pickPile : CardLoc -> Table -> Maybe ( List Card, Table )
pickPile cardLoc table =
    case cardLoc of
        CascadeLoc column row ->
            let
                takeTopN columnCards =
                    let
                        columnDepth =
                            List.length columnCards
                    in
                    List.Extra.partitionN (columnDepth - row) columnCards

                validatePileMapper ( pile, leftBehind ) =
                    case validPile pile of
                        True ->
                            Just ( pile, leftBehind )

                        False ->
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
            case mDividedPiles of
                Just ( pile, leftBehind ) ->
                    Just ( pile, { table | cascades = Array.set column leftBehind table.cascades } )

                Nothing ->
                    Nothing

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


countEmptyCascades : Column -> Int -> Table -> Int
countEmptyCascades column count table =
    if column == cascadesCount then
        count

    else
        let
            cascade =
                Array.get column table.cascades
                    |> Maybe.withDefault []

            newCount =
                if List.length cascade == 0 then
                    count + 1

                else
                    count
        in
        countEmptyCascades (column + 1) newCount table


emptyCascades : Table -> Int
emptyCascades =
    countEmptyCascades 0 0


countEmptyCells : Cell -> Int -> Table -> Int
countEmptyCells cell count table =
    if cell == cellsCount then
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
