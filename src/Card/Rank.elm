module Card.Rank exposing (increment, incrementN)

import Card exposing (Rank(..))


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
            Ace


doIncrementN : Int -> Int -> Rank -> Rank
doIncrementN i n rank =
    if i < n then
        doIncrementN (i + 1) n (increment rank)

    else
        rank


incrementN : Int -> Rank -> Rank
incrementN =
    doIncrementN 0
