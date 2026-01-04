module ManualTest exposing (..)

import Array
import Browser
import Card exposing (Card, Rank(..), Suit(..))
import Cascade exposing (Column, Row)
import Game exposing (Game, MouseEventProcessLock(..), Msg(..), State(..))
import Main exposing (Model(..), Msg(..), update)
import Move.Autosolve exposing (AutosolveOption(..))
import Table exposing (CardLoc(..), Table, TableLoc(..))
import Table.View exposing (positionFor, zIndexFor)


main : Program () Model Main.Msg
main =
    Browser.document
        { init = always init
        , view = Main.view
        , update = Main.update
        , subscriptions = always Sub.none
        }



-- {-| Use this version of init to play a curated board. -}
-- init : ( Model, Cmd Main.Msg )
-- init =
--     update (GameMsg (Autosolve ( 0, 0 ))) (InGame debugGame)


{-| Use this version of init to play an easier game.
-}
init : ( Model, Cmd Msg )
init =
    update (SetGame 88738) (MainMenu Nothing)


cascadeCard : Table -> Column -> Row -> ( Rank, Suit ) -> Card
cascadeCard table column row ( rank, suit ) =
    let
        tableLoc =
            CascadeLoc column row
    in
    { rank = rank
    , suit = suit
    , position = positionFor table tableLoc
    , zIndex = zIndexFor (Static tableLoc)
    }


debugTable : Table
debugTable =
    let
        table =
            Table.new 2 2

        cascade1 =
            [ ( King, Spades ), ( Two, Hearts ), ( Queen, Hearts ), ( Jack, Spades ), ( Ace, Spades ), ( Ten, Hearts ) ]
                |> List.indexedMap (cascadeCard table 1)
                |> List.reverse
    in
    { table
        | cascades = Array.set 1 cascade1 table.cascades |> Array.set 0 [ cascadeCard table 0 0 ( Queen, Diamonds ) ]
    }


debugGame : Game
debugGame =
    { table = debugTable
    , state = Ready
    , mouseEventProcessLock = Free
    , lastMouseDown = Nothing
    , doubleClickLast = False
    , focusedCard = Nothing
    , focusedFoundation = Nothing
    , moveHistory = []
    , number = -1
    , select = Nothing
    , autosolvePreference = AlwaysAutosolve
    }
