module Table exposing (..)

import Card exposing (Card)
import Card.View
import Deck exposing (Deck)
import Position exposing (Position)
import Table.View


type alias FoundationD =
    List Card


type alias FoundationC =
    List Card


type alias FoundationH =
    List Card


type alias FoundationS =
    List Card


type alias Table =
    { cells : List (Maybe Card)
    , cascades : List (List Card)
    , foundationD : FoundationD
    , foundationC : FoundationC
    , foundationH : FoundationH
    , foundationS : FoundationS
    }


type alias Column =
    Int


type alias Row =
    Int


type alias Depth =
    Int


type CardLoc
    = CascadeLoc Column Row
    | Hand Depth


positionFor : CardLoc -> Position
positionFor cardLoc =
    case cardLoc of
        CascadeLoc column row ->
            let
                left =
                    Table.View.cascadesOffset + toFloat column * (Card.View.width + Table.View.padding)

                top =
                    Table.View.cascadesTop + toFloat row * Table.View.pileSpacing
            in
            ( left, top )

        Hand _ ->
            ( 0, 0 )


zIndexFor : CardLoc -> Int
zIndexFor cardLoc =
    case cardLoc of
        CascadeLoc _ row ->
            row

        Hand depth ->
            150 - depth


recursiveDeal : Int -> List (List Card) -> List (List Card) -> Deck -> List (List Card)
recursiveDeal row cascades alreadyDealt deck =
    case cascades of
        [] ->
            recursiveDeal (row + 1) (List.reverse alreadyDealt) cascades deck

        cascade :: remainingCascades ->
            let
                ( topCard, restDeck ) =
                    Deck.draw deck
            in
            case topCard of
                Just card ->
                    let
                        column =
                            List.length alreadyDealt

                        cardLoc =
                            CascadeLoc column row

                        positionedCard =
                            { card | position = positionFor cardLoc, zIndex = zIndexFor cardLoc }
                    in
                    recursiveDeal row remainingCascades ((positionedCard :: cascade) :: alreadyDealt) restDeck

                Nothing ->
                    List.concat [ List.reverse alreadyDealt, cascades ]


deal : Deck -> List (List Card)
deal deck =
    recursiveDeal 0 [ [], [], [], [], [], [], [], [] ] [] deck


new : Deck -> Table
new deck =
    { cells = [ Nothing, Nothing, Nothing, Nothing ]
    , cascades = deal deck
    , foundationD = []
    , foundationC = []
    , foundationH = []
    , foundationS = []
    }
