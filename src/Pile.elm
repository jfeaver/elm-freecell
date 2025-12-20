module Pile exposing (Pile, fromCascade, validPile)

import Card exposing (Card)
import Card.Color
import Card.Rank
import Cascade exposing (Row)


{-| A "pile" is a correctly ordered stack of cards (red/black alternating and descending in rank from back to front).
-| Note that a "stack" is any stack of cards that is ordered or not.
-}
type alias Pile =
    List Card


validPilePair : Card -> Card -> Bool
validPilePair top second =
    Card.Rank.increment top.rank == second.rank && Card.Color.notColor (Card.Color.fromCard top) == Card.Color.fromCard second


validPile : List Card -> Bool
validPile cards =
    case cards of
        top :: second :: others ->
            validPilePair top second && validPile (second :: others)

        _ ->
            True


pileFinder : Row -> List Card -> Pile -> ( Row, Pile )
pileFinder row remaining pile_ =
    case remaining of
        top :: second :: [] ->
            if validPilePair top second then
                ( row - 1, List.append pile_ [ top, second ] )

            else
                ( row, List.append pile_ [ top ] )

        top :: second :: others ->
            if validPilePair top second then
                pileFinder (row - 1) (second :: others) (List.append pile_ [ top ])

            else
                ( row, List.append pile_ [ top ] )

        top :: [] ->
            -- A card by itself in a cascade is a pile
            if List.length pile_ == 0 then
                ( row, [ top ] )

            else
                ( row, pile_ )

        _ ->
            ( row, pile_ )


fromCascade : List Card -> ( Row, Pile )
fromCascade cards =
    pileFinder (List.length cards - 1) cards []
