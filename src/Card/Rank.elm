module Card.Rank exposing (decrementCard, increment, incrementN)

import Card exposing (Card, Rank(..))


increment : Rank -> Rank
increment rank =
    case rank of
        Ace ->
            Two

        Two ->
            Three

        Three ->
            Four

        Four ->
            Five

        Five ->
            Six

        Six ->
            Seven

        Seven ->
            Eight

        Eight ->
            Nine

        Nine ->
            Ten

        Ten ->
            Jack

        Jack ->
            Queen

        Queen ->
            King

        King ->
            Infinite

        Infinite ->
            Infinite


doIncrementN : Int -> Int -> Rank -> Rank
doIncrementN i n rank =
    if i < n then
        doIncrementN (i + 1) n (increment rank)

    else
        rank


incrementN : Int -> Rank -> Rank
incrementN =
    doIncrementN 0


decrement : Rank -> Rank
decrement rank =
    case rank of
        Ace ->
            King

        Two ->
            Ace

        Three ->
            Two

        Four ->
            Three

        Five ->
            Four

        Six ->
            Five

        Seven ->
            Six

        Eight ->
            Seven

        Nine ->
            Eight

        Ten ->
            Nine

        Jack ->
            Ten

        Queen ->
            Jack

        King ->
            Queen

        Infinite ->
            Infinite


decrementCard : Card -> Maybe Card
decrementCard card =
    case card.rank of
        Ace ->
            Nothing

        _ ->
            Just { card | rank = decrement card.rank }
