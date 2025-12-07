module Main exposing (..)

import Array
import Browser exposing (Document)
import Browser.Dom exposing (Element)
import Card exposing (Card, Rank(..), Suit(..))
import Card.View
import Cascade exposing (Column, Row)
import Css exposing (absolute, auto, backgroundColor, backgroundImage, backgroundRepeat, backgroundSize, contain, cursor, display, height, hex, hover, inlineBlock, int, left, margin, noRepeat, pct, pointer, position, px, relative, right, top, transform, translate2, url, width, zIndex)
import Deck exposing (Deck)
import Game exposing (Game, State(..))
import Html.Events exposing (onMouseEnter, onMouseLeave)
import Html.Events.Extra.Mouse exposing (Event, onDown, onMove, onUp)
import Html.Styled as Html exposing (Html, button, div, text)
import Html.Styled.Attributes exposing (css, fromUnstyled, id)
import Html.Styled.Events exposing (onClick)
import Move
import Pile exposing (Pile)
import Position exposing (Position)
import Random
import Table exposing (CardLoc(..), Cell, Table)
import Table.View
import Task
import Time


main : Program () Model Msg
main =
    Browser.document
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type Model
    = MainMenu
    | InGame Game


type Msg
    = SetGame Deck
    | NewGame
    | DoubleClick Position
    | MouseDown ( CardLoc, Card ) Event
    | FocusCard ( CardLoc, Card )
    | DefocusCard
    | MouseMove Event
    | MouseUp Event
    | EndMove (Result Browser.Dom.Error ( Element, Event ))
    | DetectDoubleClick Position (Result Browser.Dom.Error ( Element, Time.Posix ))


startGame : Cmd Msg
startGame =
    Random.generate SetGame Deck.randomDeck


init : ( Model, Cmd Msg )
init =
    ( MainMenu, startGame )


doubleClickUpdate : Game -> Position -> ( Model, Cmd Msg )
doubleClickUpdate game position =
    let
        automoveGame =
            game
                |> Game.autoMove
                |> (\updatedGame -> { updatedGame | doubleClickLast = True })

        -- see if click is a cascade location
        -- check if that cascade location is the last cascade card
        -- focus on that card
        mNextCardLoc =
            Table.View.locFor automoveGame.table position

        locatedTableCard tCardLoc =
            automoveGame.table
                |> Table.getTableCard tCardLoc
                |> Maybe.map (\card -> ( Table.tableCardLocToCardLoc tCardLoc, card ))

        mNextLocatedCard =
            mNextCardLoc
                |> Maybe.andThen locatedTableCard

        refocusCard card =
            update (FocusCard card) (InGame automoveGame)

        defocus =
            update DefocusCard (InGame automoveGame)
    in
    case mNextLocatedCard of
        Just locatedCard ->
            refocusCard locatedCard

        Nothing ->
            defocus


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetGame deck ->
            let
                game =
                    Game.new deck
            in
            ( InGame game, Cmd.none )

        NewGame ->
            ( model, startGame )

        DoubleClick position ->
            case model of
                InGame game ->
                    doubleClickUpdate game position

                _ ->
                    ( model, Cmd.none )

        MouseDown ( cardLoc, card ) { clientPos, button } ->
            -- Should do only these things:
            -- Record that a click happened and the position where it happened (maybe just a recycled DetectDoubleClick)
            --
            -- Elsewhere:
            -- If some threshold of pixels are moved (maybe 2?) and mouse up event is not received then we start a hand move in mouse move
            -- If some threshold of time goes by (maybe 500 ms like double click threshold) and mouse up event is not received and hand move is not started then we start a hand move
            case model of
                InGame game ->
                    let
                        updatedGame =
                            case button of
                                Html.Events.Extra.Mouse.MainButton ->
                                    Game.startMove cardLoc card clientPos game

                                _ ->
                                    game

                        getTableElement =
                            Browser.Dom.getElement "table"

                        task =
                            Task.map2 (\el time -> ( el, time )) getTableElement Time.now
                    in
                    ( InGame updatedGame, Task.attempt (DetectDoubleClick clientPos) task )

                _ ->
                    ( model, Cmd.none )

        FocusCard focusedCard ->
            case model of
                InGame game ->
                    let
                        updatedGame =
                            { game | focusedCard = Just focusedCard }
                    in
                    ( InGame updatedGame, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DefocusCard ->
            case model of
                InGame game ->
                    let
                        updatedGame =
                            { game | focusedCard = Nothing }
                    in
                    ( InGame updatedGame, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MouseMove { clientPos } ->
            case model of
                InGame game ->
                    let
                        updatedGame =
                            Game.updateMove clientPos game
                    in
                    ( InGame updatedGame, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MouseUp event ->
            let
                getTableElement =
                    Browser.Dom.getElement "table"

                elementWithEvent el =
                    ( el, event )
            in
            ( model, Task.attempt EndMove <| Task.map elementWithEvent getTableElement )

        EndMove result ->
            case model of
                InGame game ->
                    case result of
                        Ok ( tableEl, event ) ->
                            let
                                tablePosition =
                                    ( tableEl.element.x, tableEl.element.y )

                                mouseUpTablePosition =
                                    Position.diff tablePosition event.clientPos

                                mLastCardLoc =
                                    Table.View.locFor game.table mouseUpTablePosition

                                updatedGame =
                                    Game.endMove mLastCardLoc game
                            in
                            ( InGame updatedGame, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DetectDoubleClick position (Ok ( tableEl, time )) ->
            case model of
                InGame game ->
                    let
                        mouseDownDiff =
                            Time.posixToMillis time - Time.posixToMillis game.lastMouseDown

                        updatedGame =
                            { game | lastMouseDown = time, doubleClickLast = False }

                        tablePosition =
                            ( tableEl.element.x, tableEl.element.y )

                        clickTablePosition =
                            Position.diff tablePosition position
                    in
                    if mouseDownDiff <= 500 && not game.doubleClickLast then
                        update (DoubleClick clickTablePosition) (InGame updatedGame)

                    else
                        ( InGame updatedGame, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DetectDoubleClick _ _ ->
            -- If error finding table element... do nothing.
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Freecell"
    , body = List.map Html.toUnstyled (body model)
    }


body : Model -> List (Html Msg)
body model =
    case model of
        MainMenu ->
            [ header
            ]

        InGame game ->
            [ header
            , div
                [ css
                    [ backgroundColor (hex Table.View.backgroundHex)
                    , position relative
                    , margin auto
                    , width (px Table.View.width)
                    , height (px <| Table.View.height + Table.View.expandedPlayHeight game.table)
                    ]
                , onMove MouseMove |> fromUnstyled
                , onUp MouseUp |> fromUnstyled
                , id "table"
                ]
                [ cells game
                , foundations game
                , cascades game
                , activeMove game game.state
                ]
            ]


header : Html Msg
header =
    div []
        [ button [ onClick NewGame, css [ cursor pointer ] ] [ text "New Game" ]
        ]


cell : Game -> ( Cell, Maybe Card ) -> Html Msg
cell game ( cellN, maybeCard ) =
    maybeCard
        |> Maybe.map (\card -> cardView game (CellLoc cellN) card)
        |> Maybe.withDefault
            (div
                [ Table.View.cardMark
                , css
                    [ position absolute
                    , top (px Table.View.topOffset)
                    , left (px (Table.View.horizontalOffset + (Card.View.width + Table.View.padding) * toFloat cellN))
                    ]
                ]
                []
            )


cells : Game -> Html Msg
cells game =
    game.table.cells
        |> Array.toIndexedList
        |> List.map (cell game)
        |> div []


foundation : Game -> (Table -> Maybe Card) -> Suit -> Html Msg
foundation game fieldGetter suit =
    let
        suitIconWidth =
            0.53 * Card.View.width

        suitIconHeight =
            Card.View.aspectRatio * suitIconWidth

        positioning n =
            css
                [ position absolute
                , top (px Table.View.topOffset)
                , right (px (Table.View.horizontalOffset + (Card.View.width + Table.View.padding) * toFloat n))
                ]

        cardIcon filepath =
            css
                [ backgroundImage (url filepath)
                , backgroundSize contain
                , backgroundRepeat noRepeat
                , width (px suitIconWidth)
                , height (px suitIconHeight)
                , position absolute
                , left (pct 50)
                , transform (translate2 (pct -50) (pct 50))
                ]
    in
    case fieldGetter game.table of
        Just card ->
            cardView game (FoundationLoc suit) card

        Nothing ->
            div [ Table.View.cardMark, positioning (3 - Card.suitIndex suit) ]
                [ div [ cardIcon (Card.View.suitIconSrc suit) ] []
                ]


foundations : Game -> Html Msg
foundations game =
    div []
        [ foundation game .diamonds Diamonds
        , foundation game .clubs Clubs
        , foundation game .hearts Hearts
        , foundation game .spades Spades
        ]



{-
   TODO - On hover-movable pile:
   {
       box-shadow: 0px 0px 4px 3px greenyellow;
       border-radius: 5px;
       top: __
       z-index: __
   }
-}


unfocusedPileIndicator : ( Row, Pile ) -> Float -> Html Msg
unfocusedPileIndicator ( rowStart, pile ) cascadeOffset =
    let
        pileDepth =
            List.length pile

        pileHeight =
            (pileDepth |> toFloat) * Table.View.stackSpacing + Card.View.height

        indicatorTop =
            rowStart
                |> toFloat
                |> (*) Table.View.stackSpacing
                |> (+) Table.View.cascadesTop
    in
    div
        [ css
            [ position absolute
            , top (px indicatorTop)
            , left (px cascadeOffset)
            , Css.height (px pileHeight)
            , Css.width (px Card.View.width)

            -- TODO: Use UI for sizes here
            , Css.boxShadow5 (px -4) (px 0) (px 0) (px -1) (Css.rgb 95 199 199)
            , Css.borderRadius (px 5)
            , zIndex (int rowStart)
            ]
        ]
        []


focusedPileIndicator : ( Row, Pile ) -> ( CardLoc, Card ) -> List (Html Msg)
focusedPileIndicator ( row, pile ) ( cardLoc, card ) =
    []


cascade : Game -> Float -> ( Column, List Card ) -> Html Msg
cascade game cascadesOffset ( column, cards ) =
    let
        pile =
            Pile.fromCascade cards

        cascadeOffset =
            cascadesOffset + toFloat column * (Card.View.width + Table.View.padding)

        cardMark =
            div
                [ css
                    [ position absolute
                    , top (px Table.View.cascadesTop)
                    , left (px cascadeOffset)
                    ]
                , Table.View.cardMark
                ]
                []

        focusedPileIndicator_ =
            game.focusedCard
                |> Maybe.map (focusedPileIndicator pile)
                |> Maybe.withDefault []

        indicators =
            case game.state of
                PlayerMove move ->
                    if Move.startsFromCascade column move then
                        [ cardMark ]

                    else
                        [ cardMark
                        , unfocusedPileIndicator pile cascadeOffset
                        ]

                _ ->
                    [ cardMark
                    , unfocusedPileIndicator pile cascadeOffset
                    ]
    in
    div [] <|
        List.concat
            [ List.append indicators focusedPileIndicator_
            , List.indexedMap (cascadeCardView game (List.length cards) column) cards
            ]


cascades : Game -> Html Msg
cascades game =
    game.table.cascades
        |> Array.toIndexedList
        |> List.map (cascade game <| Table.View.cascadesMargin game.table)
        |> div []


cascadeCardView : Game -> Int -> Column -> Int -> Card -> Html Msg
cascadeCardView game columnDepth column inversedRow =
    let
        row =
            (columnDepth - 1) - inversedRow
    in
    cardView game (CascadeLoc column row)


cardView : Game -> CardLoc -> Card -> Html Msg
cardView game cardLoc card =
    let
        cardImage =
            css
                [ backgroundImage (url (card |> Card.View.filename))
                , backgroundSize contain
                , backgroundRepeat noRepeat
                ]

        sizing inset =
            css
                [ width (px (Card.View.width - inset))
                , height (px (Card.View.height - inset))
                ]

        positioning =
            css
                [ display inlineBlock
                , position absolute
                , top
                    (px (card.position |> Tuple.second))
                , left (px (card.position |> Tuple.first))
                , zIndex (int card.zIndex)
                ]

        getStack column row =
            game.table
                |> Table.stackTo ( column, row )
                |> Tuple.first

        twoTupleFrom thing =
            ( thing, thing )

        validPileDepth row pile =
            List.length pile <= Game.maxPileDepth row game.table

        -- A valid pile is both in order and of a movable depth
        validPile column row =
            getStack column row
                |> twoTupleFrom
                |> Tuple.mapBoth Pile.validPile (validPileDepth row)
                |> (==) ( True, True )

        hoverOnCard =
            case game.focusedCard of
                Just ( focusedCardLoc, _ ) ->
                    case focusedCardLoc of
                        CascadeLoc column row ->
                            if (cardLoc == focusedCardLoc) && validPile column row then
                                -- FIXME: when valid pile is the entire cascade and is the exact number of movable cards then the top card does not show a pointer
                                css
                                    [ hover [ cursor pointer ]
                                    ]

                            else
                                css []

                        _ ->
                            css
                                [ hover [ cursor pointer ]
                                ]

                Nothing ->
                    css []

        interaction =
            [ onDown (MouseDown ( cardLoc, card ))
            , onMouseEnter (FocusCard ( cardLoc, card ))
            , onMouseLeave DefocusCard
            ]
                |> List.map fromUnstyled

        attrs =
            List.concat [ interaction, [ cardImage, positioning, sizing 0, hoverOnCard ] ]

        cardHighlightInset =
            1

        cardHighlight =
            -- TODO: Move borderRadius and box shadow to UI size
            div
                [ positioning
                , sizing (cardHighlightInset * 2)
                , css
                    [ margin (px cardHighlightInset)
                    , Css.borderRadius (px (5 - cardHighlightInset))
                    , Css.boxShadow5 (px 0) (px 0) (px 3) (px 4) (Css.rgb 156 201 227)
                    ]
                ]
                []

        doHighlightCard =
            game.focusedCard
                |> Maybe.map (\( _, focusedCard ) -> Pile.validPile [ focusedCard, card ])
                |> Maybe.withDefault False
    in
    if doHighlightCard then
        div [] [ div attrs [], cardHighlight ]

    else
        div attrs []


activeMove : Game -> Game.State -> Html Msg
activeMove game state =
    case state of
        Game.Ready ->
            div [] []

        Game.PlayerMove move ->
            let
                toCardView depth =
                    cardView game (Hand depth)

                cardsHtml =
                    Move.indexedMap toCardView move
            in
            div [] cardsHtml
