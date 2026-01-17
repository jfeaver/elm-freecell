module Pile exposing
    ( Pile
    , cascadeRow
    , hitbox
    , validPile
    )

import Card exposing (Card)
import Card.Color
import Card.Rank
import Card.View
import Cascade exposing (Row)
import Hitbox exposing (Hitbox)


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


pileFinder : Row -> List Card -> Pile -> Row
pileFinder row remaining pile_ =
    case remaining of
        top :: second :: [] ->
            if validPilePair top second then
                row - 1

            else
                row

        top :: second :: others ->
            if validPilePair top second then
                pileFinder (row - 1) (second :: others) (List.append pile_ [ top ])

            else
                row

        _ :: [] ->
            -- A card by itself in a cascade is a pile
            if List.length pile_ == 0 then
                row

            else
                row

        _ ->
            row


{-| Returns the row where a pile begins.
-}
cascadeRow : List Card -> Row
cascadeRow cards =
    pileFinder (List.length cards - 1) cards []


hitbox_ : Pile -> Maybe Card -> Hitbox
hitbox_ pile mFirstCard =
    case mFirstCard of
        Just firstCard ->
            case pile of
                card :: others ->
                    if List.length others == 0 then
                        ( Tuple.first (Card.View.hitbox card)
                        , Tuple.second (Card.View.hitbox firstCard)
                        )

                    else
                        hitbox_ others mFirstCard

                [] ->
                    -- Pile is a single card
                    Card.View.hitbox firstCard

        Nothing ->
            case pile of
                firstCard :: others ->
                    hitbox_ others (Just firstCard)

                [] ->
                    -- Pile contains no cards (doesn't happen)
                    ( ( 0, 0 ), ( 0, 0 ) )


hitbox : Pile -> Hitbox
hitbox pile =
    hitbox_ pile Nothing
