module Card.Color exposing
    ( CardColor(..)
    , fromCard
    , fromPileTop
    , notColor
    )

import Card exposing (Card, Suit(..))


type CardColor
    = Red
    | Black


fromCard : Card -> CardColor
fromCard { suit } =
    case suit of
        Diamonds ->
            Red

        Clubs ->
            Black

        Hearts ->
            Red

        Spades ->
            Black


notColor : CardColor -> CardColor
notColor color =
    case color of
        Red ->
            Black

        Black ->
            Red


fromPile : List Card -> Maybe CardColor
fromPile pile =
    case pile of
        head :: _ ->
            Just (fromCard head)

        _ ->
            Nothing


fromPileTop : List Card -> Maybe CardColor
fromPileTop pile =
    let
        isEven =
            modBy 2 >> (==) 0

        toTop headColor =
            if isEven <| List.length pile then
                notColor headColor

            else
                headColor
    in
    Maybe.map toTop (fromPile pile)
