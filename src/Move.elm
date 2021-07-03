module Move exposing
    ( Move
    , color
    , finalize
    , getPile
    , new
    , pileDepth
    , rank
    , showingSuit
    , toCascade
    , toCell
    , toFoundation
    , update
    )

import Array
import Card exposing (Card, Rank(..), Suit(..))
import Card.Color exposing (CardColor(..))
import Card.Rank
import Position exposing (Position)
import Table exposing (CardLoc(..), Cell, Column, Table)
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

        updateCardPosition depth card =
            { card
                | position =
                    positionDiff
                        |> Position.add move.topCardStart
                        |> Position.add ( 0, Table.View.pileDepthOffset (pileDepth (Move move) - depth - 1) )
            }

        updatedPile =
            List.indexedMap updateCardPosition move.pile
    in
    Move { move | pile = updatedPile }


{-| FIXME: I want to remove this
-}
getPile : Move -> List Card
getPile (Move { pile }) =
    pile


pileDepth : Move -> Int
pileDepth (Move { pile }) =
    List.length pile


{-| Position the card, apply correct z index, and update the table
-}
finalize : Table -> Move -> Table
finalize table (Move move) =
    let
        updatedZIndex =
            Table.View.zIndexFor move.to

        updatedPosition =
            Table.View.positionFor table move.to
    in
    case move.to of
        CascadeLoc column _ ->
            let
                columnInPlace =
                    table.cascades
                        |> Array.get column
                        |> Maybe.withDefault []

                positionCard depth card =
                    { card
                        | position =
                            updatedPosition
                                |> Position.add ( 0, Table.View.pileDepthOffset (pileDepth (Move move) - depth - 1) )
                        , zIndex = updatedZIndex + pileDepth (Move move) - depth - 1
                    }

                positionedMovePile =
                    move.pile
                        |> List.indexedMap positionCard

                buildColumn =
                    -- This could be done card by card in positionedMovePile/positionCard
                    List.concat [ positionedMovePile, columnInPlace ]

                updatedCascades =
                    Array.set column buildColumn table.cascades
            in
            { table | cascades = updatedCascades }

        Hand _ ->
            table

        CellLoc cell ->
            case move.pile of
                [ card ] ->
                    let
                        updatedCard =
                            { card | position = updatedPosition, zIndex = updatedZIndex }

                        updatedCells =
                            Array.set cell (Just updatedCard) table.cells
                    in
                    { table | cells = updatedCells }

                _ ->
                    table

        FoundationLoc suit ->
            case move.pile of
                [ card ] ->
                    case suit of
                        Diamonds ->
                            { table | diamonds = Just { card | position = updatedPosition, zIndex = updatedZIndex } }

                        Clubs ->
                            { table | clubs = Just { card | position = updatedPosition, zIndex = updatedZIndex } }

                        Hearts ->
                            { table | hearts = Just { card | position = updatedPosition, zIndex = updatedZIndex } }

                        Spades ->
                            { table | spades = Just { card | position = updatedPosition, zIndex = updatedZIndex } }

                _ ->
                    table


toCascade : Column -> Table -> Move -> Move
toCascade column table (Move move) =
    let
        cascade =
            table.cascades
                |> Array.get column
                |> Maybe.withDefault []

        moveRow =
            List.length cascade
    in
    Move { move | to = CascadeLoc column moveRow }


toCell : Cell -> Move -> Move
toCell cell (Move move) =
    Move { move | to = CellLoc cell }


toFoundation : Suit -> Move -> Move
toFoundation suit (Move move) =
    Move { move | to = FoundationLoc suit }


color : Move -> CardColor
color (Move { pile }) =
    case Card.Color.fromPileTop pile of
        Just theColor ->
            theColor

        Nothing ->
            -- shouldn't happen
            Black


rank : Move -> Rank
rank (Move { pile }) =
    case pile of
        card :: _ ->
            Card.Rank.incrementN (List.length pile - 1) card.rank

        [] ->
            -- shouldn't happen
            King


showingSuit : Move -> Maybe Suit
showingSuit (Move move) =
    case move.pile of
        { suit } :: _ ->
            Just suit

        _ ->
            Nothing
