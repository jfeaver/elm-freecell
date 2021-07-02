module Table exposing (CardLoc(..), Cell, Column, Depth, Row, Table, cascadesCount, cellEmpty, cellsCount, new, pickPile, validPile)

import Array exposing (Array)
import Card exposing (Card)
import Card.Color
import Card.Rank
import List.Extra
import Maybe.Extra


type alias FoundationD =
    List Card


type alias FoundationC =
    List Card


type alias FoundationH =
    List Card


type alias FoundationS =
    List Card


type alias Table =
    { cells : Array (Maybe Card)
    , cascades : Array (List Card)
    , foundationD : FoundationD
    , foundationC : FoundationC
    , foundationH : FoundationH
    , foundationS : FoundationS
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
    , foundationD = []
    , foundationC = []
    , foundationH = []
    , foundationS = []
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


getCell : Cell -> Table -> Maybe Card
getCell cell { cells } =
    Array.get cell cells
        |> Maybe.Extra.dig


cellEmpty : Cell -> Table -> Bool
cellEmpty cell table =
    getCell cell table
        |> Maybe.Extra.isNothing
