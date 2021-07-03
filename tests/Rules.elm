module Rules exposing (..)

import Array
import Card exposing (Card, Rank(..), Suit(..))
import Deck
import Expect exposing (Expectation)
import Game exposing (Game, State(..))
import Table exposing (CardLoc(..))
import Table.View
import Test exposing (..)


suite : Test
suite =
    describe "The number of cards picked up at one time"
        [ test "the player cannot pick up an entire cascade as if the cascade were empty" <|
            \_ ->
                let
                    pickedCard =
                        Card ( 0, 0 ) 0 Hearts King

                    aPickablePile =
                        [ Card ( 0, 0 ) 0 Hearts Seven
                        , Card ( 0, 0 ) 0 Spades Eight
                        , Card ( 0, 0 ) 0 Hearts Nine
                        , Card ( 0, 0 ) 0 Spades Ten
                        , Card ( 0, 0 ) 0 Hearts Jack
                        , Card ( 0, 0 ) 0 Spades Queen
                        , pickedCard
                        ]

                    dealAPile t =
                        { t | cascades = Array.set 0 aPickablePile t.cascades }

                    table =
                        Table.View.deal (Table.new 4 8) Deck.fullDeck
                            |> dealAPile

                    cardLoc =
                        CascadeLoc 0 0

                    game =
                        Game table Ready
                in
                Expect.all
                    [ \t ->
                        -- Sanity test to make sure dealing is doing the normal thing
                        Array.get 0 t.cascades
                            |> Maybe.withDefault []
                            |> List.length
                            |> Expect.equal 7
                    , \_ ->
                        -- The largest pile that is pickable should be 5
                        Game.startMove cardLoc pickedCard ( 0, 0 ) game
                            |> .state
                            |> Expect.equal Ready
                    ]
                    table
        ]
