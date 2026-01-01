module Card.Rank exposing (decrementCard, increment, incrementN)

import Card exposing (Card, Rank(..))


toValue : Rank -> Int
toValue rank =
    case rank of
        Ace ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Jack ->
            11

        Queen ->
            12

        King ->
            13

        Infinite ->
            14


fromValue : Int -> Rank
fromValue n =
    case n of
        1 ->
            Ace

        2 ->
            Two

        3 ->
            Three

        4 ->
            Four

        5 ->
            Five

        6 ->
            Six

        7 ->
            Seven

        8 ->
            Eight

        9 ->
            Nine

        10 ->
            Ten

        11 ->
            Jack

        12 ->
            Queen

        13 ->
            King

        _ ->
            Infinite


increment : Rank -> Rank
increment rank =
    incrementN 1 rank


incrementN : Int -> Rank -> Rank
incrementN n rank =
    rank
        |> toValue
        |> (+) n
        |> fromValue


decrement : Rank -> Rank
decrement rank =
    incrementN -1 rank


decrementCard : Card -> Maybe Card
decrementCard card =
    case card.rank of
        Ace ->
            Nothing

        _ ->
            Just { card | rank = decrement card.rank }
