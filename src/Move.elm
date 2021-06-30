module Move exposing (..)

import Array
import Card exposing (Card)
import Position exposing (Position)
import Table exposing (CardLoc(..), Table)
import Table.View exposing (pileDepthOffset)


type Move
    = Move
        { from : CardLoc
        , pile : List Card
        , topCardStart : Position
        , mouseStart : Position
        , mouseCurrent : Position
        }


new : CardLoc -> Card -> List Card -> Position -> Move
new cardLoc topCard pile position =
    let
        zIndexInHand depth card =
            { card | zIndex = Table.View.zIndexFor (Hand depth) }
    in
    Move
        { from = cardLoc
        , pile = List.indexedMap zIndexInHand pile
        , topCardStart = topCard.position
        , mouseStart = position
        , mouseCurrent = position
        }


update : Position -> Move -> Move
update mousePosition (Move move) =
    let
        positionDiff =
            Position.diff move.mouseStart mousePosition

        updateCardPosition depth card =
            { card
                | position =
                    positionDiff
                        |> Position.add move.topCardStart
                        |> Position.diff ( 0, pileDepthOffset depth )
            }

        updatedPile =
            List.indexedMap updateCardPosition move.pile
    in
    Move { move | pile = updatedPile, mouseCurrent = mousePosition }


getPile : Move -> List Card
getPile (Move { pile }) =
    pile


reverse : Table -> Move -> Table
reverse table (Move move) =
    case move.from of
        CascadeLoc column _ ->
            let
                columnStillInPlace =
                    table.cascades
                        |> Array.get column
                        |> Maybe.withDefault []

                repositionCard pileDepth depth card =
                    { card
                        | position =
                            move.from
                                |> Table.View.positionFor
                                |> Position.add ( 0, toFloat (List.length columnStillInPlace + pileDepth - depth - 1) * Table.View.pileSpacing )
                        , zIndex = Table.View.zIndexFor move.from + pileDepth - depth - 1
                    }

                repositionedMovePile =
                    move.pile
                        |> List.indexedMap (repositionCard (List.length move.pile))

                resetColumn =
                    List.concat [ columnStillInPlace, repositionedMovePile ]

                updatedCascades =
                    Array.set column resetColumn table.cascades
            in
            { table | cascades = updatedCascades }

        Hand _ ->
            table
