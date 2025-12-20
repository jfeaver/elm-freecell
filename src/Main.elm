module Main exposing (..)

import Array
import Browser exposing (Document)
import Card exposing (Card, Rank(..), Suit(..))
import Card.View
import Cascade exposing (Column, Row)
import Css exposing (absolute, auto, backgroundColor, backgroundImage, backgroundRepeat, backgroundSize, borderRadius, contain, cursor, display, height, hex, hover, inlineBlock, int, left, margin, maxWidth, noRepeat, padding, pct, pointer, position, px, relative, right, top, transform, translate2, url, width, zIndex)
import Deck
import Game exposing (Game, Msg(..), State(..))
import Html.Events exposing (onMouseEnter, onMouseLeave)
import Html.Events.Extra.Mouse exposing (onDown, onMove, onUp)
import Html.Styled as Html exposing (Html, button, div, h3, text)
import Html.Styled.Attributes as HA exposing (css, disabled, fromUnstyled, id)
import Html.Styled.Events exposing (onClick, onInput)
import Modal
import Move
import Pile
import Random
import Table exposing (CardLoc(..), Cell, Table)
import Table.View
import UI


main : Program () Model Msg
main =
    Browser.document
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetGame deckSeed ->
            let
                game =
                    Game.new deckSeed
            in
            ( InGame game, Cmd.none )

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
            in
            List.append selectModal
                [ gameActions game
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
                    [ cells game
                    , foundations game
                    , cascades game
                    , activeMove game game.state
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


gameActions : Game -> Html Msg
gameActions game =
    div []
        [ button [ onClick NewGame, css [ cursor pointer ] ] [ text "New Game" ]
        , button [ onClick (GameMsg Game.Undo), css [ cursor pointer ] ] [ text "Undo" ]
        , button [ onClick SelectGame, css [ cursor pointer ] ] [ text "Select Game" ]
        , text ("Playing game number " ++ String.fromInt game.number)
        ]


cell : Game -> ( Cell, Maybe Card ) -> Html Msg
cell game ( cellN, maybeCard ) =
    maybeCard
        |> Maybe.map (\card -> cardView game (CellLoc cellN) False card)
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
            cardView game (FoundationLoc suit) False card

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
        |> Maybe.withDefault (unifiedPileIndicator details Nothing)


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


cascade : Game -> Float -> ( Column, List Card ) -> Html Msg
cascade game cascadesOffset ( column, cards ) =
    let
        ( rowStart, pile ) =
            Pile.fromCascade cards

        pileDepth =
            List.length pile

        cascadeOffset =
            cascadesOffset + toFloat column * (Card.View.width + Table.View.padding)

        pileIndicatorTop =
            rowStart
                |> toFloat
                |> (*) Table.View.stackSpacing
                |> (+) Table.View.cascadesTop

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

        mFocusedColumn =
            Game.focusedColumn game

        focused =
            -- FIXME: a focused column doesn't mean the player is focusing on the pile
            mFocusedColumn
                |> Maybe.map ((==) column)
                |> Maybe.withDefault False

        pileIndicatorDetails : PileIndicatorDetails
        pileIndicatorDetails =
            { pileDepth = List.length pile
            , rowStart = rowStart
            , indicatorTop = pileIndicatorTop
            , cascadeOffset = cascadeOffset
            , focused = focused
            }

        mPickablePileDepth =
            if focused then
                Just (Game.maxPileDepth (List.length cards - List.length pile) game.table)

            else
                Nothing

        mPileIndicator =
            if pileDepth > 1 then
                Just (pileIndicator pileIndicatorDetails mPickablePileDepth)

            else
                Nothing

        basicIndicators =
            [ cardMark ]

        indicators =
            mPileIndicator
                |> Maybe.withDefault []
                |> List.append basicIndicators
    in
    div [] <|
        List.concat
            [ indicators
            , List.indexedMap (cascadeCardView game (List.length cards) column pileDepth) cards
            ]


cascades : Game -> Html Msg
cascades game =
    game.table.cascades
        |> Array.toIndexedList
        |> List.map (cascade game <| Table.View.cascadesMargin game.table)
        |> div []


cascadeCardView : Game -> Int -> Column -> Int -> Int -> Card -> Html Msg
cascadeCardView game columnDepth column pileDepth inversedRow =
    let
        row =
            (columnDepth - 1) - inversedRow

        -- FIXME: inPile is false when the card is by itself in a cascade
        inPile =
            inversedRow < pileDepth
    in
    cardView game (CascadeLoc column row) inPile


cardView : Game -> CardLoc -> Bool -> Card -> Html Msg
cardView game cardLoc inPile card =
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
            [ onDown (MouseDown ( cardLoc, card ) >> GameMsg)
            , onMouseEnter (FocusCard ( cardLoc, card ) |> GameMsg)
            , onMouseLeave (DefocusCard |> GameMsg)
            ]
                |> List.map fromUnstyled

        attrs =
            List.concat [ interaction, [ cardImage, positioning, sizing 0, hoverOnCard ] ]

        cardHighlightInset =
            0

        cardHighlight =
            div
                [ positioning
                , sizing (cardHighlightInset * 2)
                , css
                    [ margin (px cardHighlightInset)
                    , Css.borderRadius (px (UI.indicatorRadius - cardHighlightInset))
                    , Css.boxShadow5 (px 0) (px 0) (px (UI.indicatorWidth - 1)) (px UI.indicatorWidth) UI.cardHighlightColor
                    ]
                ]
                []

        isCascade =
            case cardLoc of
                CascadeLoc _ _ ->
                    True

                _ ->
                    False

        cardShroud =
            div
                [ positioning
                , sizing 0
                , css
                    [ Css.backgroundColor (Css.rgba 0 0 0 0.17) ]
                , onMouseEnter (FocusCard ( cardLoc, card ) |> GameMsg) |> fromUnstyled
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
                |> Maybe.map (\( focusedCardLoc, _ ) -> cardLoc == focusedCardLoc)
                |> Maybe.withDefault False

        doHighlightCard =
            game.focusedCard
                |> Maybe.map (\( focusedCardLoc, focusedCard ) -> Pile.validPile [ focusedCard, card ] && not (isParentOf focusedCardLoc cardLoc))
                |> Maybe.withDefault False
    in
    if doHighlightCard then
        div [] [ div attrs [], cardHighlight ]

    else if not isCascade || inPile || isFocused then
        div attrs []

    else
        div [] [ div attrs [], cardShroud ]


activeMove : Game -> Game.State -> Html Msg
activeMove game state =
    case state of
        Game.Ready ->
            div [] []

        Game.PlayerMove move ->
            let
                toCardView depth =
                    cardView game (Hand depth) False

                cardsHtml =
                    Move.indexedMap toCardView move
            in
            div [] cardsHtml
