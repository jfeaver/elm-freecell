module Rules exposing (autosolver, pickUps, putDowns)

import Array
import Card exposing (Rank(..), Suit(..))
import Expect
import Game exposing (Game, MouseEventProcessLock(..), Msg(..), State(..))
import Move
import Move.Autosolve exposing (AutosolveOption(..))
import Table exposing (Table, TableLoc(..))
import Test exposing (..)


emptyGame : AutosolveOption -> Game
emptyGame autosolvePreference =
    { table = Table.new 4 8
    , state = Ready
    , mouseEventProcessLock = Free
    , lastMouseDown = Nothing
    , doubleClickLast = False
    , focusedCard = Nothing
    , focusedFoundation = Nothing
    , moveHistory = []
    , number = -1
    , select = Nothing
    , autosolvePreference = autosolvePreference
    }


pickUps : Test
pickUps =
    describe "The number of cards picked up at one time"
        [ test "the player cannot pick up an entire cascade as if the cascade were empty" <|
            \_ ->
                let
                    pickedCard =
                        Card.new Hearts King

                    aPickablePile =
                        [ Card.new Hearts Seven
                        , Card.new Spades Eight
                        , Card.new Hearts Nine
                        , Card.new Spades Ten
                        , Card.new Hearts Jack
                        , Card.new Spades Queen
                        , pickedCard
                        ]

                    dealAPile t =
                        { t | cascades = Array.set 0 aPickablePile t.cascades }

                    tableLoc =
                        CascadeLoc 0 0

                    newGame =
                        Game.new 1 NoAutosolve

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
                        Game.startMove ( tableLoc, pickedCard ) ( 0, 0 ) True game
                            |> .state
                            |> Expect.equal Ready
                    ]
                    table
        , test "the maximum pile depth when considering a cascade that is entirely made up of a pile is correct" <|
            \_ ->
                let
                    newTable =
                        Table.new 2 1

                    c rank suit =
                        Card.new suit rank

                    cascade =
                        [ c King Spades, c Queen Hearts, c Jack Spades ]

                    cascades =
                        Array.initialize 1 (always cascade)

                    table =
                        { newTable | cascades = cascades }

                    maxPileDepth =
                        Game.maxPileDepth table
                in
                Expect.equal maxPileDepth 3
        ]


putDowns : Test
putDowns =
    describe "The number of cards put down at one time"
        [ test "the player cannot place a max size pick uppable pile on an empty cascade" <|
            \_ ->
                let
                    pickedCard =
                        Card.new Hearts King

                    pickedPile =
                        [ Card.new Hearts Seven
                        , Card.new Spades Eight
                        , Card.new Hearts Nine
                        , Card.new Spades Ten
                        , Card.new Hearts Jack
                        , Card.new Spades Queen
                        , pickedCard
                        ]

                    pickedFrom =
                        CascadeLoc 1 7

                    dealAnEmpty t =
                        { t | cascades = Array.set 0 [] t.cascades }

                    tableLoc =
                        CascadeLoc 0 0

                    move =
                        Move.new ( pickedFrom, pickedCard ) pickedPile ( 0, 0 ) True

                    gameState =
                        PlayerMove move

                    newGame =
                        Game.new 1 NoAutosolve

                    table =
                        newGame.table
                            |> dealAnEmpty

                    game =
                        { newGame | state = gameState, table = table }
                            |> Game.endMove (Just tableLoc) move

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


autosolver : Test
autosolver =
    describe "Cards moved to foundations by the autosolver in NonSupporting mode"
        [ test "cards whose rank is greater than the minimal foundation rank of the other color by more than 2 is not auto solved while lesser cards are auto solved." <|
            \_ ->
                let
                    -- 4 diamonds when 2 clubs and 2 spades are in foundations but 5 diamonds is too big
                    threeDiamonds =
                        Card.new Diamonds Three

                    fourDiamonds =
                        Card.new Diamonds Four

                    fiveDiamonds =
                        Card.new Diamonds Five

                    twoClubs =
                        Card.new Clubs Two

                    twoSpades =
                        Card.new Spades Two

                    fourHearts =
                        Card.new Hearts Four

                    game =
                        emptyGame NonSupporting

                    table : Table
                    table =
                        game.table
                            |> (\t ->
                                    { t
                                        | diamonds = [ threeDiamonds ]
                                        , clubs = [ twoClubs ]
                                        , spades = [ twoSpades ]
                                        , hearts = [ fourHearts ]
                                        , cells = Array.fromList [ Just fiveDiamonds, Just fourDiamonds ]
                                    }
                               )

                    updatedGame =
                        { game | table = table } |> Game.update (Autosolve ( 0, 0 ) False) |> Tuple.first
                in
                updatedGame.table.diamonds
                    |> List.head
                    |> Maybe.map .rank
                    |> Expect.equal (Just Four)
        , test "cards whose rank is greater than the foundation rank of the matching color pair by more than 3 is not auto solved while lesser cards are auto solved." <|
            \_ ->
                let
                    -- 6 diamonds when 2 hearts is maximum hearts foundation card since 4 hearts needs to be supported by a black five and six needs to support that five
                    threeDiamonds =
                        Card.new Diamonds Three

                    fourDiamonds =
                        Card.new Diamonds Four

                    fiveDiamonds =
                        Card.new Diamonds Five

                    sixDiamonds =
                        Card.new Diamonds Six

                    fourClubs =
                        Card.new Clubs Four

                    fourSpades =
                        Card.new Spades Four

                    twoHearts =
                        Card.new Hearts Two

                    game =
                        emptyGame NonSupporting

                    table : Table
                    table =
                        game.table
                            |> (\t ->
                                    { t
                                        | diamonds = [ threeDiamonds ]
                                        , clubs = [ fourClubs ]
                                        , spades = [ fourSpades ]
                                        , hearts = [ twoHearts ]
                                        , cells = Array.fromList [ Just sixDiamonds, Just fiveDiamonds, Just fourDiamonds ]
                                    }
                               )

                    updatedGame =
                        { game | table = table } |> Game.update (Autosolve ( 0, 0 ) False) |> Tuple.first
                in
                updatedGame.table.diamonds
                    |> List.head
                    |> Maybe.map .rank
                    |> Expect.equal (Just Five)
        ]
