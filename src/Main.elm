module Main exposing (..)

import Array
import Browser exposing (Document)
import Card exposing (Card, Rank(..), Suit(..))
import Card.View
import Cascade exposing (Column, Row)
import Css exposing (absolute, auto, backgroundColor, backgroundImage, backgroundRepeat, backgroundSize, contain, cursor, display, height, hex, hover, inlineBlock, int, left, margin, noRepeat, pct, pointer, position, px, relative, right, top, transform, translate2, url, width, zIndex)
import Deck exposing (Deck)
import Game exposing (Game, Msg(..), State(..))
import Html.Events exposing (onMouseEnter, onMouseLeave)
import Html.Events.Extra.Mouse exposing (onDown, onMove, onUp)
import Html.Styled as Html exposing (Html, button, div, text)
import Html.Styled.Attributes exposing (css, fromUnstyled, id)
import Html.Styled.Events exposing (onClick)
import Move
import Pile exposing (Pile)
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
    = MainMenu
    | InGame Game


type Msg
    = SetGame Deck
    | NewGame
    | GameMsg Game.Msg


startGame : Cmd Msg
startGame =
    Random.generate SetGame Deck.randomDeck


init : ( Model, Cmd Msg )
init =
    ( MainMenu, startGame )


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

        GameMsg gameMsg ->
            case model of
                InGame game ->
                    Game.update gameMsg game
                        |> Tuple.mapBoth (\updatedGame -> InGame updatedGame) (Cmd.map GameMsg)

                _ ->
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


type alias PileIndicatorDetails =
    { pile : Pile
    , rowStart : Row
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
            { pile = pile
            , pileDepth = List.length pile
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
            [ onDown (MouseDown ( cardLoc, card ) >> GameMsg)
            , onMouseEnter (FocusCard ( cardLoc, card ) |> GameMsg)
            , onMouseLeave (DefocusCard |> GameMsg)
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
                    , Css.borderRadius (px (UI.indicatorRadius - cardHighlightInset))
                    , Css.boxShadow5 (px 0) (px 0) (px (UI.indicatorWidth - 1)) (px UI.indicatorWidth) UI.cardHighlightColor
                    ]
                ]
                []

        isParentOf childLoc parentCardLoc =
            case parentCardLoc of
                CascadeLoc column row ->
                    childLoc == CascadeLoc column (row + 1)

                _ ->
                    False

        doHighlightCard =
            game.focusedCard
                |> Maybe.map (\( focusedCardLoc, focusedCard ) -> Pile.validPile [ focusedCard, card ] && not (isParentOf focusedCardLoc cardLoc))
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
