module Game exposing (..)

import Card exposing (Card)
import Deck exposing (Deck)
import Move exposing (Move)
import Position exposing (Position)
import Table exposing (CardLoc, Table)
import Table.View


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
    -- TODO starting a move removes the cards from their location
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


endMove : Game -> Game
endMove game =
    case game.state of
        Ready ->
            game

        PlayerMove move ->
            Game (Move.reverse game.table move) Ready
