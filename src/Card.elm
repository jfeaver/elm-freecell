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


{-| Here's how card animation works:
When a move is finalized then the position is updated and the movingFrom is added. At the same time, we set
`activityPending` on the table to True to let our subscription know that it should subscribe to animation
frames. The inMotion attribute
starts as False and is set to False at this time (a previous move may have animated this card and we want to
stop that now). The styles show the card in the new position but translated back to its previous location.
In our animation frame subscription, we look for cards that have a movingFrom present and we now remove the
movingFrom, remove `activityPending` and set the `inMotion` to True. If `inMotion` is True then the DOM
element has a transform applied to it.
-}
type alias Card =
    { position : Position
    , movingFrom : Maybe Position
    , inMotion : Bool
    , zIndex : Int
    , suit : Suit
    , rank : Rank
    }


new : Suit -> Rank -> Card
new suit rank =
    { position = ( 0, 0 )
    , movingFrom = Nothing
    , inMotion = False
    , zIndex = 0
    , suit = suit
    , rank = rank
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


colorPairedSuit : Suit -> Suit
colorPairedSuit suit =
    case suit of
        Diamonds ->
            Hearts

        Clubs ->
            Spades

        Hearts ->
            Diamonds

        Spades ->
            Clubs


animate : Card -> Card
animate card =
    case card.movingFrom of
        Just _ ->
            { card | movingFrom = Nothing, inMotion = True }

        Nothing ->
            card


id : Card -> String
id { suit, rank } =
    let
        suitId =
            case suit of
                Spades ->
                    "spades"

                Diamonds ->
                    "diamonds"

                Clubs ->
                    "clubs"

                Hearts ->
                    "hearts"

        rankId =
            case rank of
                Ace ->
                    "ace"

                Two ->
                    "two"

                Three ->
                    "three"

                Four ->
                    "four"

                Five ->
                    "five"

                Six ->
                    "six"

                Seven ->
                    "seven"

                Eight ->
                    "eight"

                Nine ->
                    "nine"

                Ten ->
                    "ten"

                Jack ->
                    "jack"

                Queen ->
                    "queen"

                King ->
                    "king"

                Infinite ->
                    "N/A"
    in
    rankId ++ "-" ++ suitId
