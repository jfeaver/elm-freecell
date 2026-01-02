module Move exposing
    ( Move
    , autosolve
    , color
    , finalize
    , indexedMap
    , isFullCascade
    , isNoOp
    , new
    , pileDepth
    , rank
    , showingSuit
    , startsFromCascade
    , startsFromFoundation
    , to
    , toCascade
    , toCell
    , toFoundation
    , undo
    , update
    , wasAutosolved
    )

import Array
import Card exposing (Card, Rank(..), Suit(..))
import Card.Color exposing (CardColor(..))
import Card.Rank
import Cascade exposing (Column)
import Maybe.Extra
import Move.Autosolve exposing (AutosolveOption(..))
import Pile exposing (Pile)
import Position exposing (Position)
import Table exposing (CardLoc(..), Cell, Table, TableLoc(..))
import Table.View


type alias ManualMove =
    { from : CardLoc
    , to : CardLoc
    , pile : Pile
    , topCardStart : Position
    , mouseStart : Position
    , pileDepth : Int
    , rank : Rank
    , color : CardColor
    , showingSuit : Suit
    }


type alias AutosolvedMove =
    { from : CardLoc
    , card : Card
    }


type Move
    = Manual ManualMove
    | Autosolve AutosolvedMove


new : ( CardLoc, Card ) -> Pile -> Position -> Move
new ( cardLoc, topCard ) pile position =
    let
        zIndexInHand depth card =
            { card | zIndex = Table.View.zIndexFor (Hand depth) }
    in
    Manual
        { from = cardLoc
        , to = cardLoc
        , pile = List.indexedMap zIndexInHand pile
        , topCardStart = topCard.position
        , mouseStart = position
        , pileDepth = List.length pile
        , rank = topCard.rank
        , showingSuit = topCard.suit
        , color = Card.Color.fromCard topCard
        }


isAutosolveable : Table -> AutosolveOption -> Card -> Bool
isAutosolveable table autosolvePreference card =
    case Table.getTableCard (TableFoundation card.suit) table of
        Just foundationCard ->
            let
                incrementsRank =
                    Card.Rank.increment foundationCard.rank == card.rank

                cardColor =
                    Card.Color.fromCard card

                notColor =
                    Card.Color.notColor cardColor

                foundationGetter suit =
                    case suit of
                        Diamonds ->
                            table.diamonds

                        Clubs ->
                            table.clubs

                        Hearts ->
                            table.hearts

                        Spades ->
                            table.spades

                foundationRankValue : Maybe Card -> Int
                foundationRankValue mCard =
                    case Maybe.map .rank mCard of
                        Nothing ->
                            0

                        Just Ace ->
                            1

                        Just Two ->
                            2

                        Just Three ->
                            3

                        Just Four ->
                            4

                        Just Five ->
                            5

                        Just Six ->
                            6

                        Just Seven ->
                            7

                        Just Eight ->
                            8

                        Just Nine ->
                            9

                        Just Ten ->
                            10

                        Just Jack ->
                            11

                        Just Queen ->
                            12

                        Just King ->
                            13

                        Just Infinite ->
                            14

                suitValueMapper =
                    foundationGetter >> foundationRankValue

                minTupleValue ( a, b ) =
                    if a < b then
                        a

                    else
                        b

                oppositeColorFoundationsValue =
                    Card.Color.suits notColor
                        |> Tuple.mapBoth suitValueMapper suitValueMapper
                        |> minTupleValue

                sameColorFoundationValue =
                    card.suit
                        |> Card.colorPairedSuit
                        |> suitValueMapper

                oppositeColorDependentsAreFreed _ =
                    suitValueMapper card.suit < oppositeColorFoundationsValue + 2

                sameColorDependentsAreFreed _ =
                    suitValueMapper card.suit < sameColorFoundationValue + 3
            in
            case autosolvePreference of
                AlwaysAutosolve ->
                    incrementsRank

                NonSupporting ->
                    incrementsRank && oppositeColorDependentsAreFreed () && sameColorDependentsAreFreed ()

                _ ->
                    False

        Nothing ->
            card.rank == Ace


getAutosolveableCascade : Table -> AutosolveOption -> Int -> Maybe ( CardLoc, Card )
getAutosolveableCascade table autosolvePreference cascade =
    let
        cascadeCards =
            Array.get cascade table.cascades
                |> Maybe.withDefault []

        row =
            List.length cascadeCards - 1
    in
    cascadeCards
        |> List.head
        |> Maybe.Extra.filter (isAutosolveable table autosolvePreference)
        |> Maybe.map (\card -> ( CascadeLoc cascade row, card ))


autosolve : Table -> AutosolveOption -> Maybe ( Move, CardLoc )
autosolve table autosolvePreference =
    let
        getAutosolveableCell : Int -> Maybe ( CardLoc, Card )
        getAutosolveableCell cell =
            Array.get cell table.cells
                |> Maybe.withDefault Nothing
                |> Maybe.Extra.filter (isAutosolveable table autosolvePreference)
                |> Maybe.map (\card -> ( CellLoc cell, card ))

        findFirstCell cell mCard =
            if cell == table.cellsCount then
                mCard

            else if Maybe.Extra.isJust mCard then
                mCard

            else
                findFirstCell (cell + 1) (getAutosolveableCell cell)

        findFirstCascade cascade =
            if cascade == table.cascadesCount then
                Nothing

            else
                case getAutosolveableCascade table autosolvePreference cascade of
                    Just autosolveableCascade ->
                        Just autosolveableCascade

                    Nothing ->
                        findFirstCascade (cascade + 1)

        findFirstAutosolveableCard _ =
            case findFirstCell 0 Nothing of
                Just locatedCard ->
                    Just locatedCard

                Nothing ->
                    findFirstCascade 0
    in
    case autosolvePreference of
        NoAutosolve ->
            Nothing

        _ ->
            Maybe.map
                (\( cardLoc, card ) ->
                    ( Autosolve { from = cardLoc, card = card }
                    , cardLoc
                    )
                )
                (findFirstAutosolveableCard ())


wasAutosolved : Move -> Bool
wasAutosolved move =
    case move of
        Autosolve _ ->
            True

        _ ->
            False


update : Position -> Move -> Move
update mousePosition theMove =
    case theMove of
        Autosolve _ ->
            theMove

        Manual move ->
            let
                positionDiff =
                    Position.diff move.mouseStart mousePosition

                updateCardPosition depth card =
                    { card
                        | position =
                            positionDiff
                                |> Position.add move.topCardStart
                                |> Position.add ( 0, Table.View.stackOffset (pileDepth (Manual move) - depth - 1) )
                    }

                updatedPile =
                    List.indexedMap updateCardPosition move.pile
            in
            Manual { move | pile = updatedPile }


indexedMap : (Int -> Card -> a) -> Move -> List a
indexedMap fn theMove =
    case theMove of
        Manual { pile } ->
            List.indexedMap fn pile

        Autosolve { card } ->
            [ fn 0 card ]


pileDepth : Move -> Int
pileDepth theMove =
    case theMove of
        Manual move ->
            move.pileDepth

        Autosolve _ ->
            1


finalizeMoveToFoundation : Suit -> Table -> Card -> CardLoc -> Table
finalizeMoveToFoundation suit table card foundationLoc =
    let
        updatedZIndex =
            Table.View.zIndexFor foundationLoc

        updatedPosition =
            Table.View.positionFor table foundationLoc
    in
    case suit of
        Diamonds ->
            { table | diamonds = Just { card | position = updatedPosition, zIndex = updatedZIndex } }

        Clubs ->
            { table | clubs = Just { card | position = updatedPosition, zIndex = updatedZIndex } }

        Hearts ->
            { table | hearts = Just { card | position = updatedPosition, zIndex = updatedZIndex } }

        Spades ->
            { table | spades = Just { card | position = updatedPosition, zIndex = updatedZIndex } }


finalize : Table -> Move -> Table
finalize table theMove =
    case theMove of
        Manual move ->
            finalizeManualMove table move

        Autosolve { card } ->
            finalizeMoveToFoundation card.suit table card (FoundationLoc card.suit)


{-| Position the card, apply correct z index, and update the table
-}
finalizeManualMove : Table -> ManualMove -> Table
finalizeManualMove table move =
    let
        updatedZIndex =
            Table.View.zIndexFor move.to

        updatedPosition =
            Table.View.positionFor table move.to
    in
    case move.to of
        CascadeLoc column _ ->
            let
                columnInPlace =
                    table.cascades
                        |> Array.get column
                        |> Maybe.withDefault []

                currentPileDepth =
                    pileDepth (Manual move)

                positionCard depth card =
                    { card
                        | position =
                            updatedPosition
                                |> Position.add ( 0, Table.View.stackOffset (currentPileDepth - depth - 1) )
                        , zIndex = updatedZIndex + currentPileDepth - depth - 1
                    }

                positionedMovePile =
                    move.pile
                        |> List.indexedMap positionCard

                buildColumn =
                    -- This could be done card by card in positionedMovePile/positionCard
                    List.concat [ positionedMovePile, columnInPlace ]

                updatedCascades =
                    Array.set column buildColumn table.cascades
            in
            { table | cascades = updatedCascades }

        Hand _ ->
            table

        CellLoc cell ->
            case move.pile of
                [ card ] ->
                    let
                        updatedCard =
                            { card | position = updatedPosition, zIndex = updatedZIndex }

                        updatedCells =
                            Array.set cell (Just updatedCard) table.cells
                    in
                    { table | cells = updatedCells }

                _ ->
                    table

        FoundationLoc suit ->
            case move.pile of
                [ card ] ->
                    finalizeMoveToFoundation suit table card move.to

                _ ->
                    table


undo : Table -> Move -> Table
undo table theMove =
    let
        undoMove =
            case theMove of
                Manual move ->
                    Manual { move | to = move.from, from = move.to }

                Autosolve { from, card } ->
                    new ( from, card ) [ card ] card.position
    in
    finalize table undoMove


toCascade : Column -> Table -> Move -> Move
toCascade column table theMove =
    let
        cascade =
            table.cascades
                |> Array.get column
                |> Maybe.withDefault []

        moveRow =
            List.length cascade
    in
    case theMove of
        Manual move ->
            Manual { move | to = CascadeLoc column moveRow }

        Autosolve _ ->
            theMove


toCell : Cell -> Move -> Move
toCell cell theMove =
    case theMove of
        Manual move ->
            Manual { move | to = CellLoc cell }

        Autosolve _ ->
            theMove


toFoundation : Suit -> Move -> Move
toFoundation suit theMove =
    case theMove of
        Manual move ->
            Manual { move | to = FoundationLoc suit }

        Autosolve _ ->
            theMove


color : Move -> CardColor
color theMove =
    case theMove of
        Manual move ->
            move.color

        Autosolve { card } ->
            Card.Color.fromCard card


rank : Move -> Rank
rank theMove =
    case theMove of
        Manual move ->
            move.rank

        Autosolve { card } ->
            card.rank


to : Move -> CardLoc
to theMove =
    case theMove of
        Manual move ->
            move.to

        Autosolve { card } ->
            FoundationLoc card.suit


showingSuit : Move -> Suit
showingSuit theMove =
    case theMove of
        Manual move ->
            move.showingSuit

        Autosolve { card } ->
            card.suit


isFullCascade : Move -> Bool
isFullCascade theMove =
    let
        checkLoc loc =
            case loc of
                CascadeLoc _ row ->
                    row == 0

                _ ->
                    False
    in
    case theMove of
        Manual move ->
            checkLoc move.from

        Autosolve { from } ->
            checkLoc from


startsFromCascade : Column -> Move -> Bool
startsFromCascade column theMove =
    let
        checkLoc loc =
            case loc of
                CascadeLoc fromColumn _ ->
                    fromColumn == column

                _ ->
                    False
    in
    case theMove of
        Manual { from } ->
            checkLoc from

        Autosolve { from } ->
            checkLoc from


startsFromFoundation : Move -> Maybe Suit
startsFromFoundation theMove =
    case theMove of
        Manual { from } ->
            case from of
                FoundationLoc foundation ->
                    Just foundation

                _ ->
                    Nothing

        Autosolve _ ->
            Nothing


isNoOp : Move -> Bool
isNoOp theMove =
    case theMove of
        Manual move ->
            move.from == move.to

        Autosolve _ ->
            False
