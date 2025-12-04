module Game exposing
    ( Game
    , State(..)
    , autoMove
    , endMove
    , new
    , startMove
    , updateMove
    )

import Array
import Card exposing (Card, Rank(..), Suit(..))
import Card.Color
import Card.Rank
import Deck exposing (Deck)
import List.Extra
import Maybe.Extra
import Move exposing (Move)
import Position exposing (Position)
import Table exposing (CardLoc(..), Cell, Column, Table)
import Table.View exposing (TableLoc(..))
import Time


type alias Game =
    { table : Table
    , state : State
    , lastMouseDown : Time.Posix
    , doubleClickLast : Bool
    }


type State
    = Ready
    | PlayerMove Move


new : Deck -> Game
new deck =
    let
        table =
            Table.new 4 8
    in
    Game (Table.View.deal table deck) Ready (Time.millisToPosix 0) False


startMove : CardLoc -> Card -> Position -> Game -> Game
startMove cardLoc card position game =
    case game.state of
        Ready ->
            let
                move pile =
                    Move.new cardLoc card pile position
            in
            case pickMovablePile cardLoc game of
                Just ( pile, table ) ->
                    { game | table = table, state = PlayerMove <| move pile }

                Nothing ->
                    game

        _ ->
            game


updateMove : Position -> Game -> Game
updateMove position game =
    case game.state of
        Ready ->
            game

        PlayerMove lastMove ->
            { game | state = PlayerMove (Move.update position lastMove) }


autoMove : Game -> Game
autoMove game =
    case game.state of
        PlayerMove move ->
            let
                moveNothing _ =
                    if Move.pileDepth move == 0 then
                        Just move

                    else
                        Nothing

                moveToFoundation _ =
                    -- if can move to a foundation then move to foundation
                    let
                        suit =
                            Move.showingSuit move
                    in
                    if validToFoundation game.table move suit then
                        Just (Move.toFoundation suit move)

                    else
                        Nothing

                tableCascadesIndices =
                    List.range 0 (game.table.cascadesCount - 1)

                tableCellsIndices =
                    List.range 0 (game.table.cellsCount - 1)

                validCascadeFolder : Table.Column -> Maybe ( Bool, Table.Column ) -> Maybe ( Bool, Table.Column )
                validCascadeFolder column mCascade =
                    case mCascade of
                        Just ( True, foundColumn ) ->
                            -- A non-empty cascade has already been found so prefer to keep it
                            Just ( True, foundColumn )

                        Just ( False, foundColumn ) ->
                            if validToCascade game.table move column then
                                if not (Table.cascadeEmpty column game.table) then
                                    -- A new valid cascade is found which is non-empty so prefer it
                                    Just ( True, column )

                                else
                                    -- Prefer the first empty cascade found rather than this new one
                                    Just ( False, foundColumn )

                            else
                                -- This cascade isn't valid so we'll hang onto the empty cascade
                                Just ( False, foundColumn )

                        Nothing ->
                            -- No previous column is valid but maybe this one is
                            if validToCascade game.table move column then
                                -- A new valid cascade is found add True as the first Tuple term if the cascade is non-empty
                                Just ( not (Table.cascadeEmpty column game.table), column )

                            else
                                Nothing

                maybeCascade _ =
                    List.foldl validCascadeFolder Nothing tableCascadesIndices

                moveToCascade _ =
                    -- if a pile has a matching cascade then move to cascade
                    maybeCascade Nothing
                        |> Maybe.map (\( _, column ) -> Move.toCascade column game.table move)

                maybeFreeCell _ =
                    List.Extra.find (validToCell game.table move) tableCellsIndices

                moveToCell _ =
                    -- if moving a single card and an open cell exists then move to cell
                    maybeFreeCell Nothing
                        |> Maybe.map (\cell -> Move.toCell cell move)
            in
            case Maybe.Extra.try4 moveNothing moveToFoundation moveToCascade moveToCell move of
                Just updatedMove ->
                    { game | table = Move.finalize game.table updatedMove, state = Ready }

                Nothing ->
                    game

        _ ->
            game


{-| The max function takes the number of empty cascades and then the number of empty cells and returns the maximum number of cards you can move
-}
maxPileDepth : (Int -> Int -> Int) -> Table -> Int
maxPileDepth maxFn table =
    let
        emptyCascades =
            Table.emptyCascades table

        emptyCells =
            Table.emptyCells table
    in
    maxFn emptyCascades emptyCells


pickMovablePile : CardLoc -> Game -> Maybe ( List Card, Table )
pickMovablePile cardLoc game =
    Table.pickPile cardLoc game.table
        |> Maybe.andThen (maybePileMove cardLoc)


maybePileMove : CardLoc -> ( List Card, Table ) -> Maybe ( List Card, Table )
maybePileMove cardLoc ( pile, table ) =
    let
        maxCardsToMove emptyCascades emptyCells =
            case cardLoc of
                CascadeLoc _ row ->
                    if row == 0 then
                        -- Picking up a full cascade shouldn't count the active cascade as an empty one
                        2 ^ (emptyCascades - 1) * (emptyCells + 1)

                    else
                        2 ^ emptyCascades * (emptyCells + 1)

                _ ->
                    1
    in
    if List.length pile <= maxPileDepth maxCardsToMove table then
        Just ( pile, table )

    else
        Nothing


{-| While moving to an empty cascade you can't consider it to be
empty as an intermediate pile stacking zone. Additionally, if a
full cascade is being moved then that can't count as an empty
cascade either.
-}
validPileDepthOnMoveToEmptyCascade : Table -> Move -> Bool
validPileDepthOnMoveToEmptyCascade table move =
    let
        maxCardsToMove emptyCascades emptyCells =
            if Move.isFullCascade move then
                2 ^ (emptyCascades - 2) * (emptyCells + 1)

            else
                2 ^ (emptyCascades - 1) * (emptyCells + 1)
    in
    Move.pileDepth move <= maxPileDepth maxCardsToMove table


{-| For moving onto cascades
-}
validDecrement : Move -> Card -> Bool
validDecrement move card =
    Card.Rank.increment (Move.rank move) == card.rank


{-| For moving onto foundations
-}
validIncrement : Move -> Card -> Bool
validIncrement move card =
    Move.rank move == Card.Rank.increment card.rank


validToCascade : Table -> Move -> Column -> Bool
validToCascade table move column =
    let
        cascade =
            table.cascades
                |> Array.get column
                |> Maybe.withDefault []

        moveColor =
            Move.color move

        mCascadeCard =
            case cascade of
                head :: _ ->
                    Just head

                _ ->
                    Nothing
    in
    case mCascadeCard of
        Just cascadeCard ->
            (Card.Color.notColor moveColor == Card.Color.fromCard cascadeCard) && validDecrement move cascadeCard

        Nothing ->
            validPileDepthOnMoveToEmptyCascade table move


validToCell : Table -> Move -> Cell -> Bool
validToCell table move cell =
    Table.cellEmpty cell table && Move.pileDepth move == 1


validToFoundation : Table -> Move -> Suit -> Bool
validToFoundation table move suit =
    let
        foundationCard =
            case suit of
                Diamonds ->
                    table.diamonds

                Clubs ->
                    table.clubs

                Hearts ->
                    table.hearts

                Spades ->
                    table.spades

        isIncrement =
            case foundationCard of
                Just card ->
                    validIncrement move card

                Nothing ->
                    Move.rank move == Ace
    in
    Move.pileDepth move == 1 && Move.showingSuit move == suit && isIncrement


endMove : Maybe TableLoc -> Game -> Game
endMove mTableLoc game =
    case game.state of
        Ready ->
            game

        PlayerMove move ->
            let
                theMove =
                    case mTableLoc of
                        Just (TableCascade column) ->
                            if validToCascade game.table move column then
                                Move.toCascade column game.table move

                            else
                                move

                        Just (TableCell cell) ->
                            if validToCell game.table move cell then
                                Move.toCell cell move

                            else
                                move

                        Just (TableFoundation suit) ->
                            if validToFoundation game.table move suit then
                                Move.toFoundation suit move

                            else
                                move

                        Nothing ->
                            move
            in
            { game | table = Move.finalize game.table theMove, state = Ready }
