module Game exposing
    ( Game
    , State(..)
    , endMove
    , new
    , startMove
    , updateMove
    )

import Array
import Card exposing (Card, Rank(..), Suit(..))
import Card.Color
import Card.Rank
import Deck exposing (Deck)
import Move exposing (Move)
import Position exposing (Position)
import Table exposing (CardLoc, Cell, Column, Table)
import Table.View exposing (TableLoc(..))


type alias Game =
    { table : Table
    , state : State
    }


type State
    = Ready
    | PlayerMove Move


new : Deck -> Game
new deck =
    let
        table =
            Table.new
    in
    Game { table | cascades = Table.View.deal deck } Ready


startMove : CardLoc -> Card -> Position -> Game -> Game
startMove cardLoc card position game =
    case game.state of
        Ready ->
            let
                mDivided =
                    Table.pickPile cardLoc game.table

                move pile =
                    Move.new cardLoc card pile position
            in
            case mDivided of
                Just ( pile, table ) ->
                    if validPileDepth table (List.length pile) then
                        Game table (PlayerMove <| move pile)

                    else
                        game

                Nothing ->
                    game

        PlayerMove _ ->
            game


updateMove : Position -> Game -> Game
updateMove position game =
    case game.state of
        Ready ->
            game

        PlayerMove lastMove ->
            { game | state = PlayerMove (Move.update position lastMove) }


{-| The max function takes the number of empty cascades and then the number of empty cells and returns the maximum depth you can move
-}
maxPileDepth : (Int -> Int -> Int) -> Table -> Int
maxPileDepth maxFn table =
    let
        emptyCascades =
            Table.emptyCascades table

        emptyCells =
            Table.emptyCells table
    in
    maxFn emptyCascades emptyCells


validPileDepth : Table -> Int -> Bool
validPileDepth table pileDepth =
    let
        algorithm emptyCascades emptyCells =
            2 ^ emptyCascades * (emptyCells + 1)
    in
    pileDepth <= maxPileDepth algorithm table


validPileDepthOnMoveToEmptyCascade : Table -> Int -> Bool
validPileDepthOnMoveToEmptyCascade table pileDepth =
    let
        algorithm emptyCascades emptyCells =
            2 ^ (emptyCascades - 1) * (emptyCells + 1)
    in
    pileDepth <= maxPileDepth algorithm table


{-| For moving onto cascades
-}
validDecrement : Move -> Card -> Bool
validDecrement move card =
    Card.Rank.increment (Move.rank move) == card.rank


{-| For moving onto foundations
-}
validIncrement : Move -> Card -> Bool
validIncrement move card =
    Move.rank move == Card.Rank.increment card.rank


validToCascade : Table -> Move -> Column -> Bool
validToCascade table move column =
    let
        cascade =
            table.cascades
                |> Array.get column
                |> Maybe.withDefault []

        moveColor =
            Move.color move

        mCascadeColor =
            cascade
                |> Card.Color.fromPile

        mCascadeCard =
            case cascade of
                head :: _ ->
                    Just head

                _ ->
                    Nothing
    in
    case mCascadeColor of
        Just cascadeColor ->
            case mCascadeCard of
                Just cascadeCard ->
                    (Card.Color.notColor moveColor == cascadeColor) && validDecrement move cascadeCard

                Nothing ->
                    validPileDepthOnMoveToEmptyCascade table (Move.pileDepth move)

        Nothing ->
            True


validToCell : Table -> Move -> Cell -> Bool
validToCell table move cell =
    Table.cellEmpty cell table && Move.pileDepth move == 1


validToFoundation : Table -> Move -> Suit -> Bool
validToFoundation table move suit =
    let
        suitMatches =
            case Move.showingSuit move of
                Just moveSuit ->
                    moveSuit == suit

                Nothing ->
                    False

        foundationCard =
            case suit of
                Diamonds ->
                    table.diamonds

                Clubs ->
                    table.clubs

                Hearts ->
                    table.hearts

                Spades ->
                    table.spades

        isIncrement =
            case foundationCard of
                Just card ->
                    validIncrement move card

                Nothing ->
                    Move.rank move == Ace
    in
    Move.pileDepth move == 1 && suitMatches && isIncrement


endMove : Maybe TableLoc -> Game -> Game
endMove mTableLoc game =
    case game.state of
        Ready ->
            game

        PlayerMove move ->
            let
                theMove =
                    case mTableLoc of
                        Just tableLoc ->
                            case tableLoc of
                                TableCascade column ->
                                    if validToCascade game.table move column then
                                        Move.toCascade column game.table move

                                    else
                                        move

                                TableCell cell ->
                                    if validToCell game.table move cell then
                                        Move.toCell cell move

                                    else
                                        move

                                TableFoundation suit ->
                                    if validToFoundation game.table move suit then
                                        Move.toFoundation suit move

                                    else
                                        move

                        Nothing ->
                            move
            in
            Game (Move.finalize game.table theMove) Ready
