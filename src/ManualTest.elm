module ManualTest exposing (..)

import Array
import Browser
import Card exposing (Card, Rank(..), Suit(..))
import Cascade exposing (Column, Row)
import Game exposing (Game, State(..))
import Main exposing (Model(..), Msg)
import Move.Autosolve exposing (AutosolveOption(..))
import Table exposing (CardLoc(..), Table)
import Table.View exposing (positionFor, zIndexFor)


main : Program () Model Msg
main =
    Browser.document
        { init = always init
        , view = Main.view
        , update = Main.update
        , subscriptions = always Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( InGame debugGame, Cmd.none )



-- type alias Card =
--     { position : Position
--     , zIndex : Int
--     , suit : Suit
--     , rank : Rank
--     }


cascadeCard : Table -> Column -> Row -> ( Rank, Suit ) -> Card
cascadeCard table column row ( rank, suit ) =
    let
        cardLoc =
            CascadeLoc column row
    in
    { rank = rank
    , suit = suit
    , position = positionFor table cardLoc
    , zIndex = zIndexFor cardLoc
    }


debugTable : Table
debugTable =
    let
        table =
            Table.new 2 2

        cascade1 =
            [ ( Ace, Spades ), ( King, Spades ), ( Queen, Hearts ), ( Jack, Spades ), ( Ten, Hearts ) ]
                |> List.indexedMap (cascadeCard table 1)
                |> List.reverse
    in
    { table
        | cascades = Array.set 1 cascade1 table.cascades
    }


debugGame : Game
debugGame =
    { table = debugTable
    , state = Ready
    , lastMouseDown = Nothing
    , doubleClickLast = False
    , focusedCard = Nothing
    , focusedFoundation = Nothing
    , moveHistory = []
    , number = -1
    , select = Nothing
    , autosolvePreference = NoAutosolve
    }
