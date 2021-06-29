module Move exposing (..)

import Card exposing (Card)
import Position exposing (Position)
import Table exposing (CardLoc(..))


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
            { card | zIndex = Table.zIndexFor (Hand depth) }
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
            Position.diff mousePosition move.mouseStart

        updateCardPosition depth card =
            { card | position = Position.add move.topCardStart positionDiff }

        updatedPile =
            List.indexedMap updateCardPosition move.pile
    in
    Move { move | pile = updatedPile, mouseCurrent = mousePosition }


getPile : Move -> List Card
getPile (Move { pile }) =
    pile
