module Deck exposing (..)

import Card exposing (Card, Rank(..), Suit(..))
import Random
import Random.List exposing (shuffle)


type Deck
    = Deck (List Card)


{-| Make a deck of all the cards in a single suit.
Makes the deck in A-K order
fullSuit Spades == [ Card Spades Ace, Card Spades Two, Card Spades Three, Card Spades Four, Card Spades Five, Card Spades Six, Card Spades Seven, Card Spades Eight, Card Spades Nine, Card Spades Ten, Card Spades Jack, Card Spades Queen, Card Spades King ]
-}
fullSuit : Suit -> Deck
fullSuit suit =
    List.map (Card ( 0, 0 ) 0 suit) [ Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King ]
        |> Deck


doMerge : Deck -> List Deck -> Deck
doMerge (Deck merged) decks =
    case decks of
        [] ->
            Deck merged

        (Deck deck) :: rest ->
            doMerge (Deck (List.concat [ deck, merged ])) rest


merge : List Deck -> Deck
merge =
    doMerge (Deck [])


{-| A full 52-card deck in standard order.
fullDeck == Deck [ Card Spades Ace, Card Spades Two, ... ]
-}
fullDeck : Deck
fullDeck =
    let
        suits : List Suit
        suits =
            [ Spades, Diamonds, Clubs, Hearts ]
    in
    List.map fullSuit suits
        |> merge


{-| A 52-card deck in randomly shuffled order.
type Msg = ShuffleDeck Deck
Random.generate ShuffleDeck randomDeck
-}
randomDeck : Random.Generator Deck
randomDeck =
    case fullDeck of
        Deck deck ->
            Random.map Deck <| shuffle deck


draw : Deck -> ( Maybe Card, Deck )
draw (Deck deck) =
    case deck of
        [] ->
            ( Nothing, Deck deck )

        card :: rest ->
            ( Just card, Deck rest )
