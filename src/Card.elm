module Card exposing (..)

import Position exposing (Position)


type Rank
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Infinite


type Suit
    = Diamonds
    | Clubs
    | Hearts
    | Spades


type alias Card =
    { position : Position
    , zIndex : Int
    , suit : Suit
    , rank : Rank
    }


type alias CardPrototype =
    { suit : Suit
    , rank : Rank
    }


suitIndex : Suit -> Int
suitIndex suit =
    case suit of
        Diamonds ->
            0

        Clubs ->
            1

        Hearts ->
            2

        Spades ->
            3
