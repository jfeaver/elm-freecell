module Move exposing (..)

import Array
import Card exposing (Card)
import Position exposing (Position)
import Table exposing (CardLoc(..), Column, Table)
import Table.View


type Move
    = Move
        { from : CardLoc
        , to : CardLoc
        , pile : List Card
        , topCardStart : Position
        , mouseStart : Position
        }


new : CardLoc -> Card -> List Card -> Position -> Move
new cardLoc topCard pile position =
    let
        zIndexInHand depth card =
            { card | zIndex = Table.View.zIndexFor (Hand depth) }
    in
    Move
        { from = cardLoc
        , to = cardLoc
        , pile = List.indexedMap zIndexInHand pile
        , topCardStart = topCard.position
        , mouseStart = position
        }


update : Position -> Move -> Move
update mousePosition (Move move) =
    let
        positionDiff =
            Position.diff move.mouseStart mousePosition

        updateCardPosition pileDepth depth card =
            { card
                | position =
                    positionDiff
                        |> Position.add move.topCardStart
                        |> Position.add ( 0, Table.View.pileDepthOffset (pileDepth - depth) )
            }

        updatedPile =
            List.indexedMap (updateCardPosition <| List.length move.pile - 1) move.pile
    in
    Move { move | pile = updatedPile }


getPile : Move -> List Card
getPile (Move { pile }) =
    pile


finalize : Table -> Move -> Table
finalize table (Move move) =
    case move.to of
        CascadeLoc column _ ->
            let
                columnInPlace =
                    table.cascades
                        |> Array.get column
                        |> Maybe.withDefault []

                positionCard pileDepth depth card =
                    { card
                        | position =
                            move.to
                                |> Table.View.positionFor
                                |> Position.add ( 0, Table.View.pileDepthOffset (pileDepth - depth) )
                        , zIndex = Table.View.zIndexFor move.to + pileDepth - depth
                    }

                positionedMovePile =
                    move.pile
                        |> List.indexedMap (positionCard (List.length move.pile - 1))

                buildColumn =
                    -- This could be done card by card in positionedMovePile/positionCard
                    List.concat [ positionedMovePile, columnInPlace ]

                updatedCascades =
                    Array.set column buildColumn table.cascades
            in
            { table | cascades = updatedCascades }

        Hand _ ->
            table


toColumn : Column -> Table -> Move -> Move
toColumn column table (Move move) =
    let
        cascade =
            table.cascades
                |> Array.get column
                |> Maybe.withDefault []

        moveRow =
            List.length cascade
    in
    Move { move | to = CascadeLoc column moveRow }
