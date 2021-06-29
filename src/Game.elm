module Game exposing (..)

import Deck exposing (Deck)
import Move exposing (Move)
import Position exposing (Position)
import Table exposing (Table)


type alias Game =
    { table : Table
    , state : State
    }


type State
    = Ready
    | PlayerMove Move


new : Deck -> Game
new deck =
    Game (Table.new deck) Ready


startMove : Move -> Game -> Game
startMove move game =
    -- TODO starting a move removes the cards from their location
    case game.state of
        Ready ->
            { game | state = PlayerMove move }

        PlayerMove _ ->
            game


updateMove : Position -> Game -> Game
updateMove position game =
    case game.state of
        Ready ->
            game

        PlayerMove lastMove ->
            { game | state = PlayerMove (Move.update position lastMove) }


endMove : Position -> Game -> Game
endMove endingPos game =
    case game.state of
        Ready ->
            game

        PlayerMove _ ->
            { game | state = Ready }
