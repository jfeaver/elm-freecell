module Main exposing (..)

import Array
import Browser exposing (Document)
import Card exposing (Card)
import Card.View
import Css exposing (absolute, auto, backgroundColor, backgroundImage, backgroundRepeat, backgroundSize, contain, cursor, display, height, hex, inlineBlock, int, left, margin, noRepeat, pct, pointer, position, px, relative, right, top, transform, translate2, url, width, zIndex)
import Deck exposing (Deck)
import Game exposing (Game)
import Html.Events.Extra.Mouse exposing (Event, onDown, onMove, onUp)
import Html.Styled as Html exposing (Attribute, Html, button, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Move
import Random
import Table exposing (CardLoc(..), Table)
import Table.View


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
    | MouseUp


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

        MouseUp ->
            case model of
                MainMenu ->
                    ( model, Cmd.none )

                InGame game ->
                    let
                        updatedGame =
                            Game.endMove game
                    in
                    ( InGame updatedGame, Cmd.none )


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
                , onMove MouseMove |> Html.Styled.Attributes.fromUnstyled
                , onUp (always MouseUp) |> Html.Styled.Attributes.fromUnstyled
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


cell : ( Int, Maybe Card ) -> Html Msg
cell ( n, maybeCard ) =
    maybeCard
        |> Maybe.map (\card -> div [] [])
        |> Maybe.withDefault
            (div
                [ Table.View.cardMark
                , css
                    [ position absolute
                    , top (px 40)
                    , left (px (30 + (Card.View.width + Table.View.padding) * toFloat n))
                    ]
                ]
                []
            )


cells : Table -> Html Msg
cells table =
    table.cells |> Array.toIndexedList |> List.map cell |> div []


foundations : Table -> Html Msg
foundations table =
    let
        suitIconWidth =
            0.53 * Card.View.width

        suitIconHeight =
            Card.View.aspectRatio * suitIconWidth

        positioning n =
            css
                [ position absolute
                , top (px 40)
                , right (px (30 + (Card.View.width + Table.View.padding) * toFloat n))
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
    div []
        [ div [ Table.View.cardMark, positioning 3 ]
            [ div [ cardIcon "assets/diamond.svg" ] []
            ]
        , div [ Table.View.cardMark, positioning 2 ]
            [ div [ cardIcon "assets/club.svg" ] []
            ]
        , div [ Table.View.cardMark, positioning 1 ]
            [ div [ cardIcon "assets/heart.svg" ] []
            ]
        , div [ Table.View.cardMark, positioning 0 ]
            [ div [ cardIcon "assets/spade.svg" ] []
            ]
        ]


cascade : ( Int, List Card ) -> Html Msg
cascade ( n, cards ) =
    div []
        (div
            [ css
                [ position absolute
                , top (px Table.View.cascadesTop)
                , left (px (Table.View.cascadesOffset + toFloat n * (Card.View.width + Table.View.padding)))
                ]
            , Table.View.cardMark
            ]
            []
            :: List.indexedMap (cascadeCardView n) cards
        )


cascades : Table -> Html Msg
cascades table =
    table.cascades |> Array.toIndexedList |> List.map cascade |> div []


cascadeCardView : Int -> Int -> Card -> Html Msg
cascadeCardView column row =
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
            [ onDown (MouseDown ( cardLoc, card )) |> Html.Styled.Attributes.fromUnstyled
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
