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


isCard : CardPrototype -> Card -> Bool
isCard { suit, rank } card =
    card.suit == suit && card.rank == rank
