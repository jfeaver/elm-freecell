module Main exposing (..)

import Array exposing (Array)
import Browser exposing (Document)
import Browser.Events
import Card exposing (Card, Rank(..), Suit(..))
import Card.Rank
import Card.View
import Cascade exposing (Row)
import Css exposing (absolute, animationDuration, animationName, auto, backgroundColor, backgroundImage, backgroundRepeat, backgroundSize, batch, borderRadius, contain, cursor, display, height, hex, hover, inline, inlineBlock, int, left, margin, maxWidth, noRepeat, padding, pct, pointer, position, px, relative, right, top, transform, translate2, url, width, zIndex)
import Css.Animations exposing (keyframes, property)
import Css.Transitions exposing (cubicBezier, transition)
import Deck
import Game exposing (Game, Msg(..), State(..))
import Html.Events exposing (onMouseEnter, onMouseLeave, onMouseOver)
import Html.Events.Extra.Mouse exposing (onDown, onMove, onUp)
import Html.Styled as Html exposing (Html, button, div, h3, input, label, text)
import Html.Styled.Attributes as HA exposing (checked, css, disabled, for, fromUnstyled, id, name, type_)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Keyed
import Html.Styled.Lazy exposing (lazy)
import Modal
import Move
import Move.Autosolve exposing (AutosolveOption(..))
import Pile
import Position
import Random
import Table exposing (AnimationState(..), CardLoc(..), InGameCard, TableLoc(..))
import Table.View
import UI


main : Program () Model Msg
main =
    Browser.document
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = MainMenu (Maybe Deck.DeckSelect)
    | InGame Game


type Msg
    = SetGame Deck.Seed
    | NewGame
    | GameMsg Game.Msg
    | SelectGame
    | SetSelectGame String
    | CancelSelectGame


startGame : Cmd Msg
startGame =
    Random.generate SetGame Deck.initialSeed


init : ( Model, Cmd Msg )
init =
    ( MainMenu Nothing, startGame )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        MainMenu _ ->
            Sub.none

        InGame game ->
            case game.table.animation of
                AnimationPending ->
                    Browser.Events.onAnimationFrame (\posix -> GameMsg (AnimateCards posix))

                _ ->
                    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetGame deckSeed ->
            let
                autosolvePreference =
                    case model of
                        InGame g ->
                            g.autosolvePreference

                        _ ->
                            NonSupporting

                game =
                    Game.new deckSeed autosolvePreference
            in
            update (GameMsg (Autosolve ( 0, 0 ) True)) (InGame game)

        NewGame ->
            ( model, startGame )

        GameMsg gameMsg ->
            case model of
                InGame game ->
                    Game.update gameMsg game
                        |> Tuple.mapBoth (\updatedGame -> InGame updatedGame) (Cmd.map GameMsg)

                _ ->
                    ( model, Cmd.none )

        SelectGame ->
            case model of
                InGame game ->
                    ( InGame { game | select = Just Deck.initDeckSelect }, Cmd.none )

                MainMenu _ ->
                    ( MainMenu (Just Deck.initDeckSelect), Cmd.none )

        CancelSelectGame ->
            case model of
                InGame game ->
                    ( InGame { game | select = Nothing }, Cmd.none )

                MainMenu _ ->
                    ( MainMenu Nothing, Cmd.none )

        SetSelectGame inputValue ->
            let
                select =
                    Deck.deckSelectFromInput inputValue
            in
            case model of
                InGame game ->
                    ( InGame { game | select = Just select }, Cmd.none )

                MainMenu _ ->
                    ( MainMenu (Just select), Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Freecell"
    , body = List.map Html.toUnstyled (body model)
    }


body : Model -> List (Html Msg)
body model =
    case model of
        MainMenu _ ->
            []

        InGame game ->
            let
                selectModal =
                    game.select
                        |> Maybe.map selectGame
                        |> Maybe.map (\aDiv -> [ aDiv ])
                        |> Maybe.withDefault []

                cascadesPileRows =
                    game.table.cascades |> Array.map Pile.cascadeRow
            in
            List.append selectModal
                [ lazy gameActions { autosolvePreference = game.autosolvePreference, gameNumber = game.number }
                , div
                    [ css
                        [ backgroundColor (hex Table.View.backgroundHex)
                        , position relative
                        , margin auto
                        , width (px Table.View.width)
                        , height (px <| Table.View.height + Table.View.expandedPlayHeight game.table)
                        ]
                    , onMove (MouseMove >> GameMsg) |> fromUnstyled
                    , onUp (MouseUp >> GameMsg) |> fromUnstyled
                    , id "table"
                    ]
                    [ lazy cellMarks game.table.cellsCount
                    , foundationMarks
                    , lazy cascadeMarks game.table.cascadesCount
                    , Html.Styled.Keyed.node "div" [] (keyedCardsView game cascadesPileRows)
                    ]
                ]


selectGame : Deck.DeckSelect -> Html Msg
selectGame { mParseResult, input } =
    let
        mParseResult_ =
            if String.length input == 0 then
                Nothing

            else
                mParseResult

        goButton_ active seed =
            [ button
                [ disabled (not active)
                , onClick (SetGame seed)
                , css
                    (if active then
                        [ Css.backgroundColor (Css.rgb 30 190 0)
                        , Css.color (Css.rgb 255 255 255)
                        , Css.fontWeight Css.bold
                        , Css.cursor Css.pointer
                        ]

                     else
                        []
                    )
                ]
                [ text "Go!" ]
            ]

        goButton =
            case mParseResult_ of
                Just (Ok seed) ->
                    goButton_ True seed

                _ ->
                    goButton_ False 0

        inputLabel =
            [ Html.label [] [ text "Game Number:" ]
            ]

        inputHtml =
            case mParseResult_ of
                Just (Err _) ->
                    [ Html.input [ css [ Css.border3 (px 4) Css.solid (Css.rgb 255 18 18) ], HA.value input, onInput SetSelectGame ] []
                    ]

                _ ->
                    [ Html.input [ HA.value input, onInput SetSelectGame ] []
                    ]

        errorMessage =
            case mParseResult_ of
                Just (Err message) ->
                    [ Html.span [ css [ Css.color (Css.rgb 255 18 18), Css.fontStyle Css.italic ] ] [ text message ] ]

                _ ->
                    []
    in
    div []
        [ Modal.view
            []
            [ div
                [ css
                    [ backgroundColor (Css.rgb 255 255 255)
                    , padding (px 20)
                    , borderRadius (px 5)
                    , maxWidth (px 500)
                    , Css.width (pct 80)
                    ]
                ]
                [ h3 [] [ text "Which game do you want to play?" ]
                , div [] inputLabel
                , div []
                    (inputHtml ++ goButton)
                , div [] errorMessage
                , div [ css [ Css.displayFlex, Css.justifyContent Css.end ] ]
                    [ button [ onClick CancelSelectGame, css [ Css.cursor Css.pointer ] ] [ text "Cancel" ]
                    ]
                ]
            ]
        ]


autosolveOptions : AutosolveOption -> Html Msg
autosolveOptions autosolvePreference =
    div [ css [ display inline ] ]
        [ text "Auto-solver?"
        , label [ for "NoAutosolve", onClick (GameMsg (Prefer NoAutosolve)) ] [ text "Off" ]
        , input [ id "NoAutosolve", name "autosolvePreference", type_ "radio", onClick (GameMsg (Prefer NoAutosolve)), checked (autosolvePreference == NoAutosolve) ] [ text "None" ]
        , label [ for "NonSupporting", onClick (GameMsg (Prefer NonSupporting)) ] [ text "Non-Supporting" ]
        , input [ id "NonDependent", name "autosolvePreference", type_ "radio", onClick (GameMsg (Prefer NonSupporting)), checked (autosolvePreference == NonSupporting) ] [ text "Non-Dependent" ]
        , label [ for "AllAutosolve", onClick (GameMsg (Prefer AlwaysAutosolve)) ] [ text "Always" ]
        , input [ id "AllAutosolve", name "autosolvePreference", type_ "radio", onClick (GameMsg (Prefer AlwaysAutosolve)), checked (autosolvePreference == AlwaysAutosolve) ] [ text "All" ]
        ]


gameActions : { gameNumber : Deck.Seed, autosolvePreference : AutosolveOption } -> Html Msg
gameActions { gameNumber, autosolvePreference } =
    div []
        [ button [ onClick NewGame, css [ cursor pointer ] ] [ text "New Game" ]
        , button [ onClick (GameMsg Game.Restart), css [ cursor pointer ] ] [ text "Restart" ]
        , button [ onClick (GameMsg Game.Undo), css [ cursor pointer ] ] [ text "Undo" ]
        , button [ onClick SelectGame, css [ cursor pointer ] ] [ text "Select Game" ]
        , text ("Playing game number " ++ String.fromInt gameNumber)
        , autosolveOptions autosolvePreference
        ]


cellMarks : Int -> Html Msg
cellMarks cellsCount =
    let
        cellMark cell =
            div
                [ Table.View.cardMark
                , css
                    [ position absolute
                    , top (px Table.View.topOffset)
                    , left (px (Table.View.horizontalOffset + (Card.View.width + Table.View.padding) * toFloat cell))
                    ]
                ]
                []
    in
    cellsCount
        - 1
        |> List.range 0
        |> List.map cellMark
        |> div []


foundationMarks : Html Msg
foundationMarks =
    let
        positioning n =
            css
                [ position absolute
                , top (px Table.View.topOffset)
                , right (px (Table.View.horizontalOffset + (Card.View.width + Table.View.padding) * toFloat n))
                ]

        suitIconWidth =
            0.53 * Card.View.width

        suitIconHeight =
            Card.View.aspectRatio * suitIconWidth

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

        foundationMark suit =
            div ([ Table.View.cardMark, positioning (3 - Card.suitIndex suit) ] ++ foundationInteraction suit Nothing)
                [ div [ cardIcon (Card.View.suitIconSrc suit) ] []
                ]
    in
    div []
        [ foundationMark Diamonds
        , foundationMark Clubs
        , foundationMark Hearts
        , foundationMark Spades
        ]


cascadeMarks : Int -> Html Msg
cascadeMarks cascadesCount =
    let
        cascadeOffset column =
            Table.View.cascadesMargin cascadesCount + toFloat column * (Card.View.width + Table.View.padding)

        cardMark column =
            div
                [ css
                    [ position absolute
                    , top (px Table.View.cascadesTop)
                    , left (px (cascadeOffset column))
                    ]
                , Table.View.cardMark
                ]
                []
    in
    cascadesCount
        - 1
        |> List.range 0
        |> List.map cardMark
        |> div []


keyedCardsView : Game -> Array Row -> List ( String, Html Msg )
keyedCardsView game cascadesPileRows =
    let
        tableCards =
            Table.inGameCards game.table cascadesPileRows

        handCards =
            case game.state of
                PlayerMove move ->
                    Move.pile move
                        |> List.indexedMap (\i c -> { card = c, cardLoc = Hand i, inPile = True })

                _ ->
                    []
    in
    (tableCards ++ handCards)
        |> List.map (keyedCardView game)


keyedCardView : Game -> InGameCard -> ( String, Html Msg )
keyedCardView game { cardLoc, card, inPile } =
    ( Card.id card, cardView game cardLoc inPile card )


mouseDownInteraction : ( TableLoc, Card ) -> List (Html.Attribute Msg)
mouseDownInteraction locatedCard =
    [ onDown (MouseDown locatedCard >> GameMsg) |> fromUnstyled ]


foundationInteraction : Suit -> Maybe Card -> List (Html.Attribute Msg)
foundationInteraction suit mCard =
    let
        mouseDownInteraction_ =
            case mCard of
                Just card ->
                    mouseDownInteraction ( FoundationLoc suit, card )

                Nothing ->
                    []
    in
    mouseDownInteraction_
        ++ ([ onMouseEnter (FocusFoundation suit mCard |> GameMsg)
            , onMouseLeave (DefocusFoundation |> GameMsg)
            ]
                |> List.map fromUnstyled
           )


type alias PileIndicatorDetails =
    { rowStart : Row
    , pileDepth : Int
    , indicatorTop : Float
    , cascadeOffset : Float
    , focused : Bool
    }


pileIndicator : PileIndicatorDetails -> Maybe Int -> List (Html Msg)
pileIndicator details mPickablePileDepth =
    let
        mSplitPileIndicator pickablePileDepth =
            if pickablePileDepth < details.pileDepth then
                Just (splitPileIndicator pickablePileDepth details)

            else
                Nothing
    in
    mPickablePileDepth
        |> Maybe.andThen mSplitPileIndicator
        |> Maybe.withDefault []


unifiedPileIndicator : PileIndicatorDetails -> Maybe Css.Color -> List (Html Msg)
unifiedPileIndicator { pileDepth, indicatorTop, cascadeOffset, rowStart, focused } mColor =
    let
        height =
            (pileDepth - 1 |> toFloat) * Table.View.stackSpacing + Card.View.height

        defaultColor =
            if focused then
                UI.pickablePileIndicatorColor

            else
                UI.pileIndicatorColor

        color =
            mColor
                |> Maybe.withDefault defaultColor
    in
    [ div
        [ css
            [ position absolute
            , top (px indicatorTop)
            , left (px cascadeOffset)
            , Css.height (px height)
            , Css.width (px Card.View.width)
            , Css.boxShadow5 (px -UI.indicatorWidth) (px 0) (px 0) (px -1) color
            , Css.borderRadius (px UI.indicatorRadius)
            , zIndex (int rowStart)
            ]
        ]
        []
    ]


splitPileIndicator : Int -> PileIndicatorDetails -> List (Html Msg)
splitPileIndicator pickablePileDepth details =
    let
        activeColor =
            UI.pickablePileIndicatorColor

        inactiveColor =
            UI.unpickablePileIndicatorColor

        splitDepth =
            details.pileDepth - pickablePileDepth

        activePileDetails =
            { details
                | pileDepth = pickablePileDepth
                , rowStart = details.rowStart + splitDepth
                , indicatorTop = details.indicatorTop + (toFloat splitDepth * Table.View.stackSpacing)
            }
    in
    List.append (unifiedPileIndicator details (Just inactiveColor)) (unifiedPileIndicator activePileDetails (Just activeColor))


cardCss : Float -> Card -> { cardImage : List Css.Style, sizing : List Css.Style, positioning : List Css.Style, all : List Css.Style }
cardCss inset card =
    let
        cardImage =
            [ backgroundImage (url (card |> Card.View.filename))
            , backgroundSize contain
            , backgroundRepeat noRepeat
            ]

        sizing =
            [ width (px (Card.View.width - inset))
            , height (px (Card.View.height - inset))
            ]

        cardAnimationMs =
            300

        zIndex_ =
            if card.inMotion then
                batch
                    [ animationName
                        (keyframes
                            [ ( 0
                              , [ property "z-index"
                                    (String.fromInt (card.zIndex + 65))
                                ]
                              )
                            , ( 100
                              , [ property "z-index"
                                    (String.fromInt card.zIndex)
                                ]
                              )
                            ]
                        )
                    , animationDuration (Css.ms cardAnimationMs)
                    , Css.property "animation-timing-function" "steps(1, end)"
                    ]

            else
                zIndex (int card.zIndex)

        staticPositioning =
            [ display inlineBlock
            , position absolute
            , top (px (card.position |> Tuple.second))
            , left (px (card.position |> Tuple.first))
            , zIndex (int card.zIndex)
            ]

        positioningTransform =
            case card.movingFrom of
                Just movingFrom ->
                    let
                        ( dx, dy ) =
                            Position.diff card.position movingFrom
                    in
                    transform (translate2 (px dx) (px dy))

                Nothing ->
                    transform (translate2 (px 0) (px 0))

        positioningTranslation =
            if card.inMotion then
                transition [ Css.Transitions.transform3 cardAnimationMs 0 (cubicBezier 0.22 0.61 0.36 1) ]

            else
                transition []

        positioning =
            staticPositioning ++ [ positioningTransform, positioningTranslation, zIndex_ ]
    in
    { cardImage = cardImage
    , sizing = sizing
    , positioning = positioning
    , all = cardImage ++ sizing ++ positioning
    }


handCardView : Card -> Html Msg
handCardView card =
    let
        styles =
            cardCss 0 card
    in
    div [ css styles.all ] []


cardView : Game -> CardLoc -> Bool -> Card -> Html Msg
cardView game cardLoc inPile card =
    case cardLoc of
        StaticLoc tableLoc ->
            tableCardView game tableLoc inPile card

        Hand _ ->
            handCardView card


tableCardView : Game -> TableLoc -> Bool -> Card -> Html Msg
tableCardView game tableLoc inPile card =
    let
        getStack column row =
            game.table
                |> Table.stackTo ( column, row )
                |> Tuple.first

        twoTupleFrom thing =
            ( thing, thing )

        validPileDepth pile =
            List.length pile <= Game.maxPileDepth game.table

        -- A valid pile is both in order and of a movable depth
        validPile column row =
            getStack column row
                |> twoTupleFrom
                |> Tuple.mapBoth Pile.validPile validPileDepth
                |> (==) ( True, True )

        hoverOnCard =
            case game.focusedCard of
                Just ( focusedCardLoc, _ ) ->
                    case focusedCardLoc of
                        CascadeLoc column row ->
                            if (tableLoc == focusedCardLoc) && validPile column row then
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

        typicalInteraction =
            mouseDownInteraction ( tableLoc, card )
                ++ ([ onMouseEnter (FocusCard ( tableLoc, card ) |> GameMsg)
                    , onMouseLeave (DefocusCard |> GameMsg)
                    ]
                        |> List.map fromUnstyled
                   )

        interaction =
            case tableLoc of
                FoundationLoc suit ->
                    foundationInteraction suit (Just card)

                _ ->
                    typicalInteraction

        styles =
            cardCss 0 card

        attrs =
            List.concat [ interaction, [ css styles.all, hoverOnCard ] ]

        cardHighlightInset =
            0

        cardHighlight =
            div
                [ cardCss (cardHighlightInset * 2) card |> .sizing |> css
                , onMouseOver (GameMsg (FocusCard ( tableLoc, card ))) |> fromUnstyled
                , css
                    [ margin (px cardHighlightInset)
                    , Css.borderRadius (px (UI.indicatorRadius - cardHighlightInset))
                    , Css.boxShadow5 (px 0) (px 0) (px (UI.cardHighlightWidth - 1)) (px UI.cardHighlightWidth) UI.cardHighlightColor
                    ]
                ]
                []

        isCascade =
            case tableLoc of
                CascadeLoc _ _ ->
                    True

                _ ->
                    False

        cardShroud =
            div
                [ css <|
                    Css.backgroundColor (Css.rgba 0 0 0 0.17)
                        :: styles.sizing
                , onMouseEnter (FocusCard ( tableLoc, card ) |> GameMsg) |> fromUnstyled
                ]
                []

        isParentOf childLoc parentCardLoc =
            case parentCardLoc of
                CascadeLoc column row ->
                    childLoc == CascadeLoc column (row + 1)

                _ ->
                    False

        isFocused =
            game.focusedCard
                |> Maybe.map (\( focusedCardLoc, _ ) -> focusedCardLoc == tableLoc)
                |> Maybe.withDefault False

        cardIsntAncestorOfFocusedCard =
            game.focusedCard
                |> Maybe.map (\( focusedTableLoc, focusedCard ) -> Pile.validPile [ focusedCard, card ] && not (isParentOf focusedTableLoc tableLoc))
                |> Maybe.withDefault False

        isNextFoundationalCard suit =
            Table.getTableCard (FoundationLoc suit) game.table
                |> Maybe.map (\currentFoundationalCard -> currentFoundationalCard.suit == card.suit && Card.Rank.increment currentFoundationalCard.rank == card.rank)
                |> Maybe.withDefault (card.suit == suit && card.rank == Ace)

        highlightNextFoundationalCard =
            game.focusedFoundation
                |> Maybe.map isNextFoundationalCard
                |> Maybe.withDefault False

        doHighlightCard =
            cardIsntAncestorOfFocusedCard || highlightNextFoundationalCard
    in
    div attrs
        (if doHighlightCard then
            [ cardHighlight ]

         else if not isCascade || inPile || isFocused then
            []

         else
            [ cardShroud ]
        )


activeMove : Game.State -> Html Msg
activeMove state =
    case state of
        Game.Ready ->
            div [] []

        Game.PlayerMove move ->
            move
                |> Move.pile
                |> List.map handCardView
                |> div []
