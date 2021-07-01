module Table exposing (..)

import Array exposing (Array)
import Card exposing (Card)
import List.Extra


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


type alias Depth =
    Int


type CardLoc
    = CascadeLoc Column Row
    | Hand Depth


cascadesCount : Int
cascadesCount =
    8


new : Table
new =
    { cells = Array.initialize 4 (always Nothing)
    , cascades = Array.empty
    , foundationD = []
    , foundationC = []
    , foundationH = []
    , foundationS = []
    }


pickPile : CardLoc -> Table -> ( List Card, Table )
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

                ( pile, leftBehind ) =
                    -- TODO : Cascades should be its own type so that I don't have to add the default always
                    table.cascades
                        |> Array.get column
                        |> Maybe.withDefault []
                        |> takeTopN

                updatedTable =
                    { table | cascades = Array.set column leftBehind table.cascades }
            in
            ( pile, updatedTable )

        Hand _ ->
            ( [], table )
