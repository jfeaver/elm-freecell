module Card.View exposing (..)

import Card exposing (Card, Rank(..), Suit(..))
import UI


rankName : Rank -> String
rankName rank =
    case rank of
        Ace ->
            "A"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

        Ten ->
            "10"

        Jack ->
            "J"

        Queen ->
            "Q"

        King ->
            "K"

        Infinite ->
            ""


suitName : Suit -> String
suitName suit =
    case suit of
        Diamonds ->
            "diamond"

        Clubs ->
            "club"

        Hearts ->
            "heart"

        Spades ->
            "spade"


filename : Card -> String
filename card =
    "assets/Playing_card_" ++ suitName card.suit ++ "_" ++ rankName card.rank ++ ".svg"


width : Float
width =
    75 |> UI.zoomed |> UI.roundToWhole


aspectRatio : Float
aspectRatio =
    2.5 / 2


height : Float
height =
    width * aspectRatio |> UI.roundToWhole


suitIconSrc : Suit -> String
suitIconSrc suit =
    "assets/" ++ suitName suit ++ ".svg"


{-| The svg rendered card doesn't quite go to the bottom of its requested area.
When the image is rendered with a height of 75 pixels then the render falls
short of that height by about 0.3 pixels
-}
shortBy : Float
shortBy =
    0.3 |> UI.zoomed
