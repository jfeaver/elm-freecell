module Move exposing
    ( Move
    , color
    , finalize
    , indexedMap
    , isFullCascade
    , new
    , pileDepth
    , rank
    , showingSuit
    , startsFromCascade
    , toCascade
    , toCell
    , toFoundation
    , update
    )

import Array
import Card exposing (Card, Rank(..), Suit(..))
import Card.Color exposing (CardColor(..))
import Cascade exposing (Column)
import Pile exposing (Pile)
import Position exposing (Position)
import Table exposing (CardLoc(..), Cell, Table)
import Table.View


type Move
    = Move
        { from : CardLoc
        , to : CardLoc
        , pile : Pile
        , topCardStart : Position
        , mouseStart : Position
        , pileDepth : Int
        , rank : Rank
        , color : CardColor
        , showingSuit : Suit
        }


new : CardLoc -> Card -> Pile -> Position -> Move
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
        , pileDepth = List.length pile
        , rank = topCard.rank
        , showingSuit = topCard.suit
        , color = Card.Color.fromCard topCard
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
                        |> Position.add ( 0, Table.View.stackOffset (pileDepth (Move move) - depth - 1) )
            }

        updatedPile =
            List.indexedMap updateCardPosition move.pile
    in
    Move { move | pile = updatedPile }


indexedMap : (Int -> Card -> a) -> Move -> List a
indexedMap fn (Move { pile }) =
    List.indexedMap fn pile


pileDepth : Move -> Int
pileDepth (Move move) =
    move.pileDepth


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
                                |> Position.add ( 0, Table.View.stackOffset (pileDepth (Move move) - depth - 1) )
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
color (Move move) =
    move.color


rank : Move -> Rank
rank (Move move) =
    move.rank


showingSuit : Move -> Suit
showingSuit (Move move) =
    move.showingSuit


isFullCascade : Move -> Bool
isFullCascade (Move move) =
    case move.from of
        CascadeLoc _ row ->
            row == 0

        _ ->
            False


startsFromCascade : Column -> Move -> Bool
startsFromCascade column (Move { from }) =
    case from of
        CascadeLoc fromColumn _ ->
            fromColumn == column

        _ ->
            False
