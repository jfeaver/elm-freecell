module Game exposing (..)

import Array
import Card exposing (Card)
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
                ( pile, table ) =
                    Table.pickPile cardLoc game.table

                move =
                    Move.new cardLoc card pile position
            in
            Game table (PlayerMove move)

        PlayerMove _ ->
            game


updateMove : Position -> Game -> Game
updateMove position game =
    case game.state of
        Ready ->
            game

        PlayerMove lastMove ->
            { game | state = PlayerMove (Move.update position lastMove) }


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

        mCascadeRank =
            case cascade of
                [] ->
                    Nothing

                head :: _ ->
                    Just head.rank

        moveRank =
            Move.rank move
    in
    case mCascadeColor of
        Just cascadeColor ->
            case mCascadeRank of
                Just cascadeRank ->
                    (Card.Color.notColor moveColor == cascadeColor) && (Card.Rank.increment moveRank == cascadeRank)

                Nothing ->
                    True

        Nothing ->
            True


validToCell : Table -> Move -> Cell -> Bool
validToCell table move cell =
    Table.cellEmpty cell table && Move.isOneCard move


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
                                        Move.toColumn column game.table move

                                    else
                                        move

                                TableCell cell ->
                                    if validToCell game.table move cell then
                                        Move.toCell cell move

                                    else
                                        move

                        Nothing ->
                            move
            in
            Game (Move.finalize game.table theMove) Ready
