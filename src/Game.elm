module Game exposing
    ( Game
    , MouseDownDetail
    , Msg(..)
    , State(..)
    , endMove
    , focusedPile
    , maxPileDepth
    , new
    , startMove
    , update
    )

import Array
import Browser.Dom exposing (Element)
import Card exposing (Card, Rank(..), Suit(..))
import Card.Color
import Card.Rank
import Cascade exposing (Column, Row)
import Deck
import Html.Events.Extra.Mouse exposing (Event)
import List.Extra
import Maybe.Extra
import Move exposing (Move)
import Move.Autosolve exposing (AutosolveOption(..))
import Position exposing (Position)
import Table exposing (CardLoc(..), Cell, Table, TableLoc(..))
import Table.View
import Task
import Time


{-| The position here is absolute. Since it is used for starting a drag and drop Move and that isn't relative to the table element.
-}
type alias MouseDownDetail =
    { time : Time.Posix
    , position : Position
    , locatedCard : ( CardLoc, Card )
    , mouseUpReceived : Bool
    }


type State
    = Ready
    | PlayerMove Move


type alias Game =
    { table : Table
    , state : State
    , lastMouseDown : Maybe MouseDownDetail
    , doubleClickLast : Bool
    , focusedCard : Maybe ( CardLoc, Card )
    , focusedFoundation : Maybe Suit
    , moveHistory : List Move
    , number : Deck.Seed
    , select : Maybe Deck.DeckSelect
    , autosolvePreference : AutosolveOption
    }


type Msg
    = MouseDown ( CardLoc, Card ) Event
    | MouseMove Event
    | MouseUp Event
    | FocusCard ( CardLoc, Card )
    | DefocusCard
    | FocusFoundation Suit (Maybe Card)
    | DefocusFoundation
    | EndMove (Result Browser.Dom.Error ( Element, Event ))
    | RecordMouseDownTAndP Position ( CardLoc, Card ) (Result Browser.Dom.Error ( Element, Time.Posix ))
    | Undo
    | Restart
    | Prefer AutosolveOption
    | Autosolve


new : Deck.Seed -> AutosolveOption -> Game
new deckSeed autosolvePreference =
    let
        table =
            Table.new 4 8

        deck =
            Deck.fromSeed deckSeed
    in
    { table = Table.View.deal table deck
    , state = Ready
    , lastMouseDown = Nothing
    , doubleClickLast = False
    , focusedCard = Nothing
    , focusedFoundation = Nothing
    , moveHistory = []
    , number = deckSeed
    , select = Nothing
    , autosolvePreference = autosolvePreference
    }
        |> autosolve


doubleClickUpdate : Game -> MouseDownDetail -> Position -> ( Game, Cmd Msg )
doubleClickUpdate game mouseDownDetail tablePosition =
    let
        autoMoveGame =
            game
                |> startMove mouseDownDetail.locatedCard mouseDownDetail.position
                |> autoMove
                |> Maybe.withDefault game
                |> (\updatedGame -> { updatedGame | doubleClickLast = True })

        -- see if click is a cascade location
        -- check if that cascade location is the last cascade card
        -- focus on that card
        mNextCardLoc =
            Table.View.locFor autoMoveGame.table tablePosition

        mLocatedTableCard tCardLoc =
            autoMoveGame.table
                |> Table.getTableCard tCardLoc
                |> Maybe.map (\card -> ( Table.tableCardLocToCardLoc tCardLoc, card ))

        mNextLocatedCard =
            mNextCardLoc
                |> Maybe.andThen mLocatedTableCard

        refocusCard card =
            update (FocusCard card) autoMoveGame

        defocus _ =
            update DefocusCard autoMoveGame

        updateFocus =
            case mNextLocatedCard of
                Just locatedCard ->
                    refocusCard locatedCard

                Nothing ->
                    defocus ()
    in
    updateFocus |> Tuple.mapSecond (\_ -> performMessage Autosolve)


performMessage : msg -> Cmd msg
performMessage msg =
    Task.perform identity (Task.succeed msg)


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        MouseDown locatedCard { clientPos, button } ->
            -- Should do only these things:
            -- Record that a click happened and the position where it happened (maybe just a recycled DetectDoubleClick)
            --
            -- Elsewhere:
            -- If some threshold of pixels are moved (maybe 2?) and mouse up event is not received then we start a hand move in mouse move
            case button of
                Html.Events.Extra.Mouse.MainButton ->
                    let
                        getTableElement =
                            Browser.Dom.getElement "table"

                        task =
                            Task.map2 (\el time -> ( el, time )) getTableElement Time.now
                    in
                    ( game, Task.attempt (RecordMouseDownTAndP clientPos locatedCard) task )

                _ ->
                    ( game, Cmd.none )

        MouseMove { clientPos } ->
            ( updateMouseMove clientPos game, Cmd.none )

        MouseUp event ->
            let
                getTableElement =
                    Browser.Dom.getElement "table"

                elementWithEvent el =
                    ( el, event )

                withMouseUp lastMouseDown =
                    { lastMouseDown | mouseUpReceived = True }

                updatedGame =
                    case game.lastMouseDown of
                        Just mouseDownDetail ->
                            { game | lastMouseDown = Just (withMouseUp mouseDownDetail) }

                        Nothing ->
                            game
            in
            ( updatedGame, Task.attempt EndMove <| Task.map elementWithEvent getTableElement )

        FocusCard focusedCard ->
            ( { game | focusedCard = Just focusedCard }, Cmd.none )

        DefocusCard ->
            ( { game | focusedCard = Nothing }, Cmd.none )

        FocusFoundation suit mCard ->
            case mCard of
                Just card ->
                    ( { game | focusedFoundation = Just suit, focusedCard = Just ( FoundationLoc card.suit, card ) }, Cmd.none )

                Nothing ->
                    ( { game | focusedFoundation = Just suit }, Cmd.none )

        DefocusFoundation ->
            ( { game | focusedFoundation = Nothing, focusedCard = Nothing }, Cmd.none )

        EndMove result ->
            case result of
                Ok ( tableEl, event ) ->
                    case game.state of
                        Ready ->
                            ( game, Cmd.none )

                        PlayerMove move ->
                            let
                                tablePosition =
                                    ( tableEl.element.x, tableEl.element.y )

                                mouseUpTablePosition =
                                    Position.diff tablePosition event.clientPos

                                mLastCardLoc =
                                    Table.View.locFor game.table mouseUpTablePosition
                            in
                            ( endMove mLastCardLoc move game, performMessage Autosolve )

                Err _ ->
                    ( game, Cmd.none )

        RecordMouseDownTAndP position locatedCard (Ok ( tableEl, time )) ->
            let
                mouseDownDiff lastMouseDownTime =
                    Time.posixToMillis time - Time.posixToMillis lastMouseDownTime

                tableElPosition =
                    ( tableEl.element.x, tableEl.element.y )

                tablePosition =
                    Position.diff tableElPosition position

                mouseDownDetail : MouseDownDetail
                mouseDownDetail =
                    { time = time
                    , position = position
                    , locatedCard = locatedCard
                    , mouseUpReceived = False
                    }

                recordMouseDown =
                    { game | lastMouseDown = Just mouseDownDetail, doubleClickLast = False }
            in
            case game.lastMouseDown of
                Just lastMouseDown ->
                    if mouseDownDiff lastMouseDown.time <= 500 && not game.doubleClickLast then
                        doubleClickUpdate recordMouseDown mouseDownDetail tablePosition

                    else
                        ( recordMouseDown, Cmd.none )

                Nothing ->
                    ( recordMouseDown, Cmd.none )

        RecordMouseDownTAndP _ _ _ ->
            -- If error finding table element... do nothing.
            ( game, Cmd.none )

        Undo ->
            case game.moveHistory of
                theMove :: others ->
                    let
                        pickupCards =
                            pickMovablePile (Move.to theMove) game

                        updatedAutosolvePreference =
                            if Move.wasAutosolved theMove then
                                NoAutosolve

                            else
                                game.autosolvePreference
                    in
                    case pickupCards of
                        Just ( _, table ) ->
                            ( { game
                                | table = Move.undo table theMove
                                , moveHistory = others
                                , autosolvePreference = updatedAutosolvePreference
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( game, Cmd.none )

                _ ->
                    ( game, Cmd.none )

        Restart ->
            ( new game.number game.autosolvePreference, Cmd.none )

        Prefer autosolvePreference ->
            ( { game | autosolvePreference = autosolvePreference }, performMessage Autosolve )

        Autosolve ->
            ( autosolve game, Cmd.none )


autosolve : Game -> Game
autosolve game =
    let
        mMove _ =
            Move.autosolve game.table game.autosolvePreference

        pickupCardForMove ( move, cardLoc ) =
            case Table.pickPile cardLoc game.table of
                Just ( _, table ) ->
                    ( move, table )

                Nothing ->
                    ( move, game.table )

        putdownCardOnFoundation ( theMove, table ) =
            Move.finalize table theMove

        updateGame move table =
            { game | table = table, moveHistory = move :: game.moveHistory }
    in
    case game.autosolvePreference of
        NoAutosolve ->
            game

        _ ->
            case mMove () of
                Just ( move, cardLoc ) ->
                    ( move, cardLoc )
                        |> pickupCardForMove
                        |> putdownCardOnFoundation
                        |> updateGame move
                        |> autosolve

                Nothing ->
                    game


startMove : ( CardLoc, Card ) -> Position -> Game -> Game
startMove ( cardLoc, card ) position game =
    case game.state of
        Ready ->
            let
                move pile =
                    Move.new ( cardLoc, card ) pile position
            in
            case pickMovablePile cardLoc game of
                Just ( pile, table ) ->
                    { game | table = table, state = PlayerMove <| move pile }

                Nothing ->
                    game

        _ ->
            game


updateMouseMove : Position -> Game -> Game
updateMouseMove position game =
    case game.state of
        Ready ->
            -- start move if mouse down detail is present, a mouse up hasn't been received, and the pixel diff is greater than some threshold
            case game.lastMouseDown of
                Just mouseDownDetail ->
                    let
                        movement =
                            Position.diff mouseDownDetail.position position
                                |> Position.magnitude

                        cardLoc =
                            mouseDownDetail.locatedCard |> Tuple.first

                        card =
                            mouseDownDetail.locatedCard |> Tuple.second
                    in
                    if not mouseDownDetail.mouseUpReceived && movement > 2 then
                        startMove ( cardLoc, card ) mouseDownDetail.position game

                    else
                        game

                Nothing ->
                    game

        PlayerMove lastMove ->
            { game | state = PlayerMove (Move.update position lastMove) }


{-| autoMove only returns a game if a move succeeded.
-}
autoMove : Game -> Maybe Game
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
                    -- if can move to a foundation then move to foundation (unless it's already there then try moving elsewhere)
                    let
                        suit =
                            Move.showingSuit move
                    in
                    if Maybe.Extra.isNothing (Move.startsFromFoundation move) && validToFoundation game.table move suit then
                        Just (Move.toFoundation suit move)

                    else
                        Nothing

                tableCascadesIndices =
                    List.range 0 (game.table.cascadesCount - 1)

                tableCellsIndices =
                    List.range 0 (game.table.cellsCount - 1)

                validCascadeFolder : Column -> Maybe ( Bool, Column ) -> Maybe ( Bool, Column )
                validCascadeFolder column mCascade =
                    if Move.startsFromCascade column move then
                        -- Avoid moving to the current location
                        mCascade

                    else
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
            Maybe.Extra.try4 moveNothing moveToFoundation moveToCascade moveToCell move
                |> Maybe.andThen
                    (\updatedMove ->
                        if Move.isNoOp updatedMove then
                            Nothing

                        else
                            Just
                                { game
                                    | table = Move.finalize game.table updatedMove
                                    , moveHistory = updatedMove :: game.moveHistory
                                    , state = Ready
                                }
                    )

        _ ->
            Nothing


{-| The max function takes the number of empty cascades and then the number of empty cells and returns the maximum number of cards you can move
-}
maxPileDepthAlgorithm : (Int -> Int -> Int) -> Table -> Int
maxPileDepthAlgorithm maxFn table =
    let
        emptyCascades =
            Table.emptyCascades table

        emptyCells =
            Table.emptyCells table
    in
    maxFn emptyCascades emptyCells


{-| This is intended to be used as the first argument to `maxPileDepthAlgorithm`. We only ever don't use this algorithm when the player is finalizing a stack move to an empty cascade (See validPileDepthOnMoveToEmptyCascade).
-}
maxCascadesPileDepth : Row -> Int -> Int -> Int
maxCascadesPileDepth row emptyCascades emptyCells =
    2 ^ emptyCascades * (emptyCells + 1)


{-| Given the row that you're picking up from (since picking from row 0 creates an empty
-| cascade that shouldn't be counted) and the table state, this returns the maximum number
-| of cards in a pile that may be moved at once.
-}
maxPileDepth : Row -> Table -> Int
maxPileDepth row table =
    maxPileDepthAlgorithm (maxCascadesPileDepth row) table


pickMovablePile : CardLoc -> Game -> Maybe ( List Card, Table )
pickMovablePile cardLoc game =
    let
        mPileMove ( pile, table ) =
            maybePileMove cardLoc pile game.table

        withUpdatedTable table pile =
            ( pile, table )
    in
    game.table
        |> Table.pickPile cardLoc
        |> Maybe.andThen (\( pile, table ) -> mPileMove ( pile, table ) |> Maybe.map (withUpdatedTable table))


maybePileMove : CardLoc -> List Card -> Table -> Maybe (List Card)
maybePileMove cardLoc pile table =
    let
        moveMax =
            case cardLoc of
                CascadeLoc _ row ->
                    maxPileDepth row table

                _ ->
                    1
    in
    if List.length pile <= moveMax then
        Just pile

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
    Move.pileDepth move <= maxPileDepthAlgorithm maxCardsToMove table


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


endMove : Maybe TableLoc -> Move -> Game -> Game
endMove mTableLoc move game =
    let
        theMove =
            case mTableLoc of
                Just (TableCascade column _) ->
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

        maybeAppendedMoveHistory =
            if Move.isNoOp theMove then
                game.moveHistory

            else
                theMove :: game.moveHistory
    in
    { game
        | table = Move.finalize game.table theMove
        , state = Ready
        , moveHistory = maybeAppendedMoveHistory
    }


focusedPile : Column -> Int -> Int -> Game -> Bool
focusedPile column pileDepth columnDepth game =
    case game.focusedCard of
        Just ( CascadeLoc focusedColumn focusedRow, _ ) ->
            column == focusedColumn && pileDepth > (columnDepth - focusedRow - 1)

        _ ->
            False
