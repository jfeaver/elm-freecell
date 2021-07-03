module Main exposing (..)

import Array
import Browser exposing (Document)
import Browser.Dom exposing (Element)
import Card exposing (Card, Rank(..), Suit(..))
import Card.View
import Css exposing (absolute, auto, backgroundColor, backgroundImage, backgroundRepeat, backgroundSize, contain, cursor, display, height, hex, inlineBlock, int, left, margin, noRepeat, pct, pointer, position, px, relative, right, top, transform, translate2, url, width, zIndex)
import Deck exposing (Deck)
import Game exposing (Game)
import Html.Events.Extra.Mouse exposing (Event, onDown, onMove, onUp)
import Html.Styled as Html exposing (Attribute, Html, button, div, text)
import Html.Styled.Attributes exposing (css, fromUnstyled, id)
import Html.Styled.Events exposing (onClick)
import Move
import Position
import Random
import Table exposing (CardLoc(..), Cell, Column, Table)
import Table.View
import Task


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
    = NewGame Deck
    | RequestNewGame
    | MouseDown ( CardLoc, Card ) Event
    | MouseMove Event
    | MouseUp Event
    | EndMove (Result Browser.Dom.Error ( Element, Event ))


startGame : Cmd Msg
startGame =
    Random.generate NewGame Deck.randomDeck


init : ( Model, Cmd Msg )
init =
    ( MainMenu, startGame )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame deck ->
            let
                game =
                    Game.new deck
            in
            ( InGame game, Cmd.none )

        RequestNewGame ->
            ( model, startGame )

        MouseDown ( cardLoc, card ) { clientPos, button } ->
            case model of
                MainMenu ->
                    ( model, Cmd.none )

                InGame game ->
                    let
                        updatedGame =
                            case button of
                                Html.Events.Extra.Mouse.MainButton ->
                                    Game.startMove cardLoc card clientPos game

                                _ ->
                                    game
                    in
                    ( InGame updatedGame, Cmd.none )

        MouseMove { clientPos } ->
            case model of
                MainMenu ->
                    ( model, Cmd.none )

                InGame game ->
                    let
                        updatedGame =
                            Game.updateMove clientPos game
                    in
                    ( InGame updatedGame, Cmd.none )

        MouseUp event ->
            let
                getElement =
                    Browser.Dom.getElement "table"

                elementWithEvent el =
                    ( el, event )
            in
            ( model, Task.attempt EndMove <| Task.map elementWithEvent getElement )

        EndMove result ->
            case model of
                MainMenu ->
                    ( model, Cmd.none )

                InGame game ->
                    case result of
                        Ok ( element, event ) ->
                            let
                                tablePosition =
                                    ( element.element.x, element.element.y )

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
                    , height (px Table.View.height)
                    ]
                , onMove MouseMove |> fromUnstyled
                , onUp MouseUp |> fromUnstyled
                , id "table"
                ]
                [ cells game.table
                , foundations game.table
                , cascades game.table
                , activeMove game.state
                ]
            ]


header : Html Msg
header =
    div []
        [ button [ onClick RequestNewGame, css [ cursor pointer ] ] [ text "New Game" ]
        ]


cell : ( Cell, Maybe Card ) -> Html Msg
cell ( cellN, maybeCard ) =
    maybeCard
        |> Maybe.map (\card -> cardView (CellLoc cellN) card)
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


cells : Table -> Html Msg
cells table =
    table.cells
        |> Array.toIndexedList
        |> List.map cell
        |> div []


foundation : Table -> (Table -> Maybe Card) -> Suit -> Html Msg
foundation table fieldGetter suit =
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
    case fieldGetter table of
        Just card ->
            cardView (FoundationLoc suit) card

        Nothing ->
            div [ Table.View.cardMark, positioning (3 - Card.suitIndex suit) ]
                [ div [ cardIcon (Card.View.suitIconSrc suit) ] []
                ]


foundations : Table -> Html Msg
foundations table =
    div []
        [ foundation table .diamonds Diamonds
        , foundation table .clubs Clubs
        , foundation table .hearts Hearts
        , foundation table .spades Spades
        ]


cascade : Float -> ( Column, List Card ) -> Html Msg
cascade cascadesOffset ( column, cards ) =
    div []
        (div
            [ css
                [ position absolute
                , top (px Table.View.cascadesTop)
                , left (px (cascadesOffset + toFloat column * (Card.View.width + Table.View.padding)))
                ]
            , Table.View.cardMark
            ]
            []
            :: List.indexedMap (cascadeCardView (List.length cards) column) cards
        )


cascades : Table -> Html Msg
cascades table =
    table.cascades |> Array.toIndexedList |> List.map (cascade <| Table.View.cascadesOffset table) |> div []


cascadeCardView : Int -> Column -> Int -> Card -> Html Msg
cascadeCardView columnDepth column inversedRow =
    let
        row =
            (columnDepth - 1) - inversedRow
    in
    cardView (CascadeLoc column row)


cardView : CardLoc -> Card -> Html Msg
cardView cardLoc card =
    let
        cardImage =
            css
                [ backgroundImage (url (card |> Card.View.filename))
                , backgroundSize contain
                , backgroundRepeat noRepeat
                , width (px Card.View.width)
                , height (px Card.View.height)
                , zIndex (int card.zIndex)
                ]

        positioning =
            css
                [ display inlineBlock
                , position absolute
                , top (px (card.position |> Tuple.second))
                , left (px (card.position |> Tuple.first))
                ]

        dragNDrop =
            [ onDown (MouseDown ( cardLoc, card )) |> fromUnstyled
            ]

        attrs =
            List.concat [ dragNDrop, [ cardImage, positioning ] ]
    in
    div attrs []


activeMove : Game.State -> Html Msg
activeMove state =
    case state of
        Game.Ready ->
            div [] []

        Game.PlayerMove move ->
            let
                toCardView depth =
                    cardView (Hand depth)

                cardsHtml =
                    move
                        |> Move.getPile
                        |> List.indexedMap toCardView
            in
            div [] cardsHtml
