module Move exposing
    ( Move
    , autosolve
    , color
    , finalize
    , hitbox
    , isDestructiveAutoSolveMove
    , isFullCascade
    , isNoOp
    , new
    , pickLoc
    , pile
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
    , validToCascade
    , validToCell
    , validToFoundation
    , wasAutosolved
    )

import Array
import Card exposing (Card, Rank(..), Suit(..))
import Card.Color exposing (CardColor(..))
import Card.Rank
import Card.View
import Cascade exposing (Column)
import Hitbox exposing (Hitbox)
import Maybe.Extra
import Move.Autosolve exposing (AutosolveOption(..))
import Pile exposing (Pile)
import Position exposing (Position)
import Table exposing (AnimationState(..), CardLoc(..), Cell, Table, TableLoc(..))
import Table.View


type alias DirectedMove =
    { from : TableLoc
    , to : TableLoc
    , pile : Pile
    , topCardStart : Position
    , mouseStart : Position
    , pileDepth : Int
    , rank : Rank
    , color : CardColor
    , showingSuit : Suit
    }


type alias AutosolvedMove =
    { from : TableLoc
    , card : Card
    }


type Move
    = DragAndDrop DirectedMove
    | AutoMove DirectedMove
    | Autosolve AutosolvedMove


{-| Create a new DirectedMove type move.
-}
new : ( TableLoc, Card ) -> Pile -> Position -> Bool -> Move
new ( tableLoc, topCard ) cardPile position isAutoMove =
    let
        zIndexInHand depth card =
            { card | zIndex = Table.View.zIndexFor ( Hand depth, card ) }

        constructor =
            if isAutoMove then
                AutoMove

            else
                DragAndDrop
    in
    constructor
        { from = tableLoc
        , to = tableLoc
        , pile = List.indexedMap zIndexInHand cardPile
        , topCardStart = topCard.position
        , mouseStart = position
        , pileDepth = List.length cardPile
        , rank = topCard.rank
        , showingSuit = topCard.suit
        , color = Card.Color.fromCard topCard
        }


isAutosolveable : Table -> AutosolveOption -> Card -> Bool
isAutosolveable table autosolvePreference card =
    case Table.getTableCard (FoundationLoc card.suit) table of
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

                foundationRankValue : List Card -> Int
                foundationRankValue cards =
                    case cards of
                        topCard :: _ ->
                            case topCard.rank of
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

                        [] ->
                            0

                suitFoundationValue =
                    foundationGetter >> foundationRankValue

                minTupleValue ( a, b ) =
                    if a < b then
                        a

                    else
                        b

                oppositeColorFoundationsValue =
                    Card.Color.suits notColor
                        |> Tuple.mapBoth suitFoundationValue suitFoundationValue
                        |> minTupleValue

                sameColorFoundationValue =
                    card.suit
                        |> Card.colorPairedSuit
                        |> suitFoundationValue

                oppositeColorDependentsAreFreed _ =
                    suitFoundationValue card.suit < oppositeColorFoundationsValue + 2

                sameColorDependentsAreFreed _ =
                    suitFoundationValue card.suit < sameColorFoundationValue + 3
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


getAutosolveableCascade : Table -> AutosolveOption -> Int -> Maybe ( TableLoc, Card )
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


autosolve : Table -> AutosolveOption -> Maybe ( Move, TableLoc )
autosolve table autosolvePreference =
    let
        getAutosolveableCell : Int -> Maybe ( TableLoc, Card )
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
                    ( Autosolve { from = cardLoc, card = { card | movingFrom = Just card.position } }
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


isDestructiveAutoSolveMove : Move -> Move -> Bool
isDestructiveAutoSolveMove move manualMove =
    let
        checkPreviousMove previousMove =
            case move of
                Autosolve autosolveMove ->
                    previousMove.to == autosolveMove.from

                _ ->
                    False
    in
    case manualMove of
        Autosolve _ ->
            False

        DragAndDrop previousMove ->
            checkPreviousMove previousMove

        AutoMove previousMove ->
            checkPreviousMove previousMove


{-| This updates drag and drop moves as the mouse moves around.
-}
update : Position -> Move -> Move
update mousePosition theMove =
    case theMove of
        DragAndDrop move ->
            let
                positionDiff =
                    Position.diff move.mouseStart mousePosition

                updateCardPosition depth card =
                    { card
                        | position =
                            positionDiff
                                |> Position.add move.topCardStart
                                |> Position.add ( 0, Table.View.stackOffset (pileDepth (DragAndDrop move) - depth - 1) )
                    }

                updatedPile =
                    List.indexedMap updateCardPosition move.pile
            in
            DragAndDrop { move | pile = updatedPile }

        _ ->
            theMove


pile : Move -> Pile
pile theMove =
    case theMove of
        DragAndDrop move ->
            move.pile

        AutoMove move ->
            move.pile

        Autosolve { card } ->
            [ card ]


pileDepth : Move -> Int
pileDepth theMove =
    case theMove of
        DragAndDrop move ->
            move.pileDepth

        AutoMove move ->
            move.pileDepth

        Autosolve _ ->
            1


finalizeMoveToFoundation : Suit -> Table -> Card -> TableLoc -> Table
finalizeMoveToFoundation suit table card foundationLoc =
    let
        updatedZIndex =
            Table.View.zIndexFor ( StaticLoc foundationLoc, card )

        updatedPosition =
            Table.View.positionFor table foundationLoc

        updatedCard =
            { card | position = updatedPosition, zIndex = updatedZIndex }
    in
    case suit of
        Diamonds ->
            { table | diamonds = updatedCard :: table.diamonds, animation = AnimationPending }

        Clubs ->
            { table | clubs = updatedCard :: table.clubs, animation = AnimationPending }

        Hearts ->
            { table | hearts = updatedCard :: table.hearts, animation = AnimationPending }

        Spades ->
            { table | spades = updatedCard :: table.spades, animation = AnimationPending }


finalize : Table -> Move -> Table
finalize table theMove =
    case theMove of
        DragAndDrop move ->
            finalizeManualMove table False move

        AutoMove move ->
            finalizeManualMove table True move

        Autosolve { card } ->
            finalizeMoveToFoundation card.suit table card (FoundationLoc card.suit)


{-| Position the card, apply correct z index, and update the table
-}
finalizeManualMove : Table -> Bool -> DirectedMove -> Table
finalizeManualMove table isAnimated move =
    let
        updatedZIndex card =
            Table.View.zIndexFor ( StaticLoc move.to, card )

        updatedPosition =
            Table.View.positionFor table move.to

        mMovingFrom card =
            if isAnimated then
                Just card.position

            else
                Nothing
    in
    case move.to of
        CascadeLoc column _ ->
            let
                columnInPlace =
                    table.cascades
                        |> Array.get column
                        |> Maybe.withDefault []

                currentPileDepth =
                    pileDepth (DragAndDrop move)

                positionCard depth card =
                    { card
                        | position =
                            updatedPosition
                                |> Position.add ( 0, Table.View.stackOffset (currentPileDepth - depth - 1) )
                        , zIndex = updatedZIndex card + currentPileDepth - depth - 1
                        , movingFrom = mMovingFrom card
                        , inMotion = False
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
            { table | cascades = updatedCascades, animation = AnimationPending }

        CellLoc cell ->
            case move.pile of
                [ card ] ->
                    let
                        updatedCard =
                            { card | position = updatedPosition, zIndex = updatedZIndex card, movingFrom = mMovingFrom card }

                        updatedCells =
                            Array.set cell (Just updatedCard) table.cells
                    in
                    { table | cells = updatedCells, animation = AnimationPending }

                _ ->
                    table

        FoundationLoc suit ->
            case move.pile of
                [ card ] ->
                    finalizeMoveToFoundation suit table { card | movingFrom = mMovingFrom card } move.to

                _ ->
                    table


undo : Table -> Move -> Table
undo table theMove =
    let
        undoMove =
            case theMove of
                DragAndDrop move ->
                    AutoMove { move | to = move.from, from = move.to }

                AutoMove move ->
                    AutoMove { move | to = move.from, from = move.to }

                Autosolve { from, card } ->
                    new ( from, card ) [ card ] card.position True
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

        updateMove move =
            { move | to = CascadeLoc column moveRow }
    in
    case theMove of
        DragAndDrop move ->
            updateMove move |> DragAndDrop

        AutoMove move ->
            updateMove move |> AutoMove

        Autosolve _ ->
            theMove


toCell : Cell -> Move -> Move
toCell cell theMove =
    case theMove of
        DragAndDrop move ->
            DragAndDrop { move | to = CellLoc cell }

        AutoMove move ->
            AutoMove { move | to = CellLoc cell }

        Autosolve _ ->
            theMove


toFoundation : Suit -> Move -> Move
toFoundation suit theMove =
    case theMove of
        DragAndDrop move ->
            DragAndDrop { move | to = FoundationLoc suit }

        AutoMove move ->
            AutoMove { move | to = FoundationLoc suit }

        Autosolve _ ->
            theMove


color : Move -> CardColor
color theMove =
    case theMove of
        DragAndDrop move ->
            move.color

        AutoMove move ->
            move.color

        Autosolve { card } ->
            Card.Color.fromCard card


rank : Move -> Rank
rank theMove =
    case theMove of
        DragAndDrop move ->
            move.rank

        AutoMove move ->
            move.rank

        Autosolve { card } ->
            card.rank


to : Move -> TableLoc
to theMove =
    case theMove of
        DragAndDrop move ->
            move.to

        AutoMove move ->
            move.to

        Autosolve { card } ->
            FoundationLoc card.suit


showingSuit : Move -> Suit
showingSuit theMove =
    case theMove of
        DragAndDrop move ->
            move.showingSuit

        AutoMove move ->
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
        DragAndDrop move ->
            checkLoc move.from

        AutoMove move ->
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
        DragAndDrop { from } ->
            checkLoc from

        AutoMove { from } ->
            checkLoc from

        Autosolve { from } ->
            checkLoc from


startsFromFoundation : Move -> Maybe Suit
startsFromFoundation theMove =
    let
        checkLoc loc =
            case loc of
                FoundationLoc foundation ->
                    Just foundation

                _ ->
                    Nothing
    in
    case theMove of
        DragAndDrop { from } ->
            checkLoc from

        AutoMove { from } ->
            checkLoc from

        Autosolve _ ->
            Nothing


isNoOp : Move -> Bool
isNoOp theMove =
    case theMove of
        DragAndDrop move ->
            move.from == move.to

        AutoMove move ->
            move.from == move.to

        Autosolve _ ->
            False


hitbox : Move -> Hitbox
hitbox theMove =
    case theMove of
        DragAndDrop move ->
            Pile.hitbox move.pile

        AutoMove move ->
            Pile.hitbox move.pile

        Autosolve autosolvedMove ->
            Card.View.hitbox autosolvedMove.card


{-| While moving to an empty cascade you can't consider it to be
empty as an intermediate pile stacking zone. Additionally, if a
full cascade is being moved then that can't count as an empty
cascade either.
-}
validPileDepthOnMoveToEmptyCascade : Table -> Move -> Bool
validPileDepthOnMoveToEmptyCascade table move =
    let
        maxCardsToMove emptyCascades emptyCells =
            if isFullCascade move then
                2 ^ (emptyCascades - 2) * (emptyCells + 1)

            else
                2 ^ (emptyCascades - 1) * (emptyCells + 1)
    in
    pileDepth move <= Table.maxPileDepthAlgorithm maxCardsToMove table


{-| For moving onto cascades
-}
validDecrement : Move -> Card -> Bool
validDecrement move card =
    Card.Rank.increment (rank move) == card.rank


{-| For moving onto foundations
-}
validIncrement : Move -> Card -> Bool
validIncrement move card =
    rank move == Card.Rank.increment card.rank


validToCascade : Table -> Move -> Column -> Bool
validToCascade table move column =
    let
        cascade =
            table.cascades
                |> Array.get column
                |> Maybe.withDefault []

        moveColor =
            color move

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
    Table.cellEmpty cell table && pileDepth move == 1


validToFoundation : Table -> Move -> Suit -> Bool
validToFoundation table move suit =
    let
        foundation =
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
            case List.head foundation of
                Just card ->
                    validIncrement move card

                Nothing ->
                    rank move == Ace
    in
    pileDepth move == 1 && showingSuit move == suit && isIncrement


{-| Given a list of table locations that might be the intended move target, pick one that is valid and most ideal.
-}
pickLoc : Move -> Table -> List TableLoc -> Maybe TableLoc
pickLoc move table locs =
    let
        considerOnlyValidLocs loc validLocs =
            case loc of
                FoundationLoc suit ->
                    if validToFoundation table move suit then
                        loc :: validLocs

                    else
                        validLocs

                CascadeLoc column _ ->
                    if validToCascade table move column then
                        loc :: validLocs

                    else
                        validLocs

                CellLoc cell ->
                    if validToCell table move cell then
                        loc :: validLocs

                    else
                        validLocs

        pickBestLoc loc mLoc =
            case loc of
                FoundationLoc _ ->
                    Just loc

                CascadeLoc _ _ ->
                    case mLoc of
                        Just (FoundationLoc _) ->
                            mLoc

                        _ ->
                            Just loc

                CellLoc _ ->
                    case mLoc of
                        Just (FoundationLoc _) ->
                            mLoc

                        Just (CascadeLoc _ _) ->
                            mLoc

                        Nothing ->
                            Just loc

                        _ ->
                            mLoc
    in
    locs
        |> List.foldl considerOnlyValidLocs []
        |> List.foldl pickBestLoc Nothing
