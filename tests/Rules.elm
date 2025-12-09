module Rules exposing
    ( pickUps
    , putDowns
    )

import Array
import Card exposing (Card, Rank(..), Suit(..))
import Deck
import Expect
import Game exposing (State(..))
import Move
import Table exposing (CardLoc(..), TableLoc(..))
import Test exposing (..)


pickUps : Test
pickUps =
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

                    cardLoc =
                        CascadeLoc 0 0

                    newGame =
                        Game.new Deck.fullDeck

                    table =
                        newGame.table
                            |> dealAPile

                    game =
                        { newGame | table = table }
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
                        Game.startMove ( cardLoc, pickedCard ) ( 0, 0 ) game
                            |> .state
                            |> Expect.equal Ready
                    ]
                    table
        ]


putDowns : Test
putDowns =
    describe "The number of cards put down at one time"
        [ test "the player cannot place a max size pick uppable pile on an empty cascade" <|
            \_ ->
                let
                    pickedCard =
                        Card ( 0, 0 ) 0 Hearts King

                    pickedPile =
                        [ Card ( 0, 0 ) 0 Hearts Seven
                        , Card ( 0, 0 ) 0 Spades Eight
                        , Card ( 0, 0 ) 0 Hearts Nine
                        , Card ( 0, 0 ) 0 Spades Ten
                        , Card ( 0, 0 ) 0 Hearts Jack
                        , Card ( 0, 0 ) 0 Spades Queen
                        , pickedCard
                        ]

                    pickedFrom =
                        CascadeLoc 1 7

                    dealAnEmpty t =
                        { t | cascades = Array.set 0 [] t.cascades }

                    tableLoc =
                        TableCascade 0 0

                    move =
                        PlayerMove (Move.new ( pickedFrom, pickedCard ) pickedPile ( 0, 0 ))

                    newGame =
                        Game.new Deck.fullDeck

                    table =
                        newGame.table
                            |> dealAnEmpty

                    game =
                        { newGame | state = move, table = table }
                            |> Game.endMove (Just tableLoc)

                    finalTable =
                        case game.state of
                            PlayerMove finalMove ->
                                Move.finalize table finalMove

                            _ ->
                                table
                in
                Array.get 0 finalTable.cascades
                    |> Maybe.withDefault [ pickedCard ]
                    |> List.length
                    |> Expect.equal 0
        ]
