module Table exposing
    ( AnimationState(..)
    , CardLoc(..)
    , Cell
    , Depth
    , HitboxQuadMap
    , InGameCard
    , Table
    , TableLoc(..)
    , animateCards
    , callAsTableLocWithDefault
    , cascadeDepth
    , cascadeEmpty
    , cellEmpty
    , emptyCascades
    , emptyCells
    , getTableCard
    , inGameCards
    , maxPileDepthAlgorithm
    , new
    , pickPile
    , stackTo
    )

import Array exposing (Array)
import Card exposing (Card, Suit(..))
import Cascade exposing (Column, Row)
import Hitbox exposing (Hitbox)
import Html exposing (table)
import List.Extra
import Maybe.Extra
import Pile exposing (Pile, validPile)


{-| The head of the list is the showing card.
-}
type alias Foundation =
    List Card


type AnimationState
    = StaticScene
    | AnimationPending


type alias Table =
    { cells : Array (Maybe Card)
    , cellsCount : Int
    , cascades : Array (List Card)
    , cascadesCount : Int
    , diamonds : Foundation
    , clubs : Foundation
    , hearts : Foundation
    , spades : Foundation
    , hitboxes : HitboxQuadMap
    , animation : AnimationState
    }


type alias HitboxQuadMap =
    { foundations : List ( Hitbox, TableLoc )
    , cells : List ( Hitbox, TableLoc )
    , cascadesHalf : List ( Hitbox, TableLoc )
    , cascadesOthers : List ( Hitbox, TableLoc )
    }


type alias Cell =
    Int


type alias Depth =
    Int


type CardLoc
    = Hand Depth
    | StaticLoc TableLoc


type TableLoc
    = CascadeLoc Column Row
    | CellLoc Cell
    | FoundationLoc Suit


new : Int -> Int -> Table
new cellsCount cascadesCount =
    { cells = Array.initialize cellsCount (always Nothing)
    , cellsCount = cellsCount
    , cascades = Array.initialize cascadesCount (always [])
    , cascadesCount = cascadesCount
    , diamonds = []
    , clubs = []
    , hearts = []
    , spades = []
    , hitboxes = { foundations = [], cells = [], cascadesHalf = [], cascadesOthers = [] }
    , animation = StaticScene
    }


stackHeight : List Card -> Row -> Int
stackHeight cards row =
    let
        columnDepth =
            List.length cards
    in
    columnDepth - row


{-| Creates two stacks from the card's cascade and returns them as two lists in a tuple.
-| The first stack is the card specified and all cards below it. The second stack is the
-| rest of the cards above the specified card in the cascade.
-}
stackTo : ( Column, Row ) -> Table -> ( List Card, List Card )
stackTo ( column, row ) table =
    table.cascades
        |> Array.get column
        |> Maybe.withDefault []
        |> (\cards -> List.Extra.partitionN (stackHeight cards row) cards)


cascadeDepth : Column -> Table -> Maybe Int
cascadeDepth column table =
    table.cascades
        |> Array.get column
        |> Maybe.map List.length


pickPile : TableLoc -> Table -> Maybe ( Pile, Table )
pickPile tableLoc table =
    case tableLoc of
        CascadeLoc column row ->
            let
                validatePileMapper ( pile, leftBehind ) =
                    if validPile pile then
                        Just ( pile, leftBehind )

                    else
                        Nothing

                -- Generates data like: Maybe (pile, leftBehind)
                mDividedPiles : Maybe ( Pile, List Card )
                mDividedPiles =
                    stackTo ( column, row ) table
                        |> validatePileMapper
            in
            mDividedPiles
                |> Maybe.map
                    (\( pile, leftBehind ) ->
                        ( pile, { table | cascades = Array.set column leftBehind table.cascades } )
                    )

        CellLoc cell ->
            let
                hasCardMapper card =
                    ( [ card ], { table | cells = Array.set cell Nothing table.cells } )
            in
            Maybe.map hasCardMapper (getCell cell table)

        FoundationLoc suit ->
            case suit of
                Diamonds ->
                    case table.diamonds of
                        card :: others ->
                            Just ( [ card ], { table | diamonds = others } )

                        [] ->
                            Nothing

                Clubs ->
                    case table.clubs of
                        card :: others ->
                            Just ( [ card ], { table | clubs = others } )

                        [] ->
                            Nothing

                Hearts ->
                    case table.hearts of
                        card :: others ->
                            Just ( [ card ], { table | hearts = others } )

                        [] ->
                            Nothing

                Spades ->
                    case table.spades of
                        card :: others ->
                            Just ( [ card ], { table | spades = others } )

                        [] ->
                            Nothing


getCell : Cell -> Table -> Maybe Card
getCell cell { cells } =
    Array.get cell cells
        |> Maybe.Extra.dig


callAsTableLocWithDefault : a -> (TableLoc -> a) -> CardLoc -> a
callAsTableLocWithDefault default fn cardLoc =
    case cardLoc of
        Hand _ ->
            default

        StaticLoc tableLoc ->
            fn tableLoc


getTableCard : TableLoc -> Table -> Maybe Card
getTableCard tableLoc table =
    let
        getStackCard : Row -> List Card -> Maybe Card
        getStackCard row stack =
            if row + 1 == List.length stack then
                List.head stack

            else
                List.tail stack
                    |> Maybe.andThen (\rest -> getStackCard row rest)
    in
    case tableLoc of
        CascadeLoc column row ->
            table.cascades
                |> Array.get column
                |> Maybe.andThen (getStackCard row)

        CellLoc cell ->
            getCell cell table

        FoundationLoc foundation ->
            case foundation of
                Diamonds ->
                    List.head table.diamonds

                Clubs ->
                    List.head table.clubs

                Hearts ->
                    List.head table.hearts

                Spades ->
                    List.head table.spades


cellEmpty : Cell -> Table -> Bool
cellEmpty cell table =
    getCell cell table
        |> Maybe.Extra.isNothing


cascadeEmpty : Column -> Table -> Bool
cascadeEmpty column table =
    table.cascades
        |> Array.get column
        |> Maybe.withDefault []
        |> List.length
        |> (==) 0


countEmptyCascades : Column -> Int -> Table -> Int
countEmptyCascades column count table =
    if column == table.cascadesCount then
        count

    else
        let
            newCount =
                if cascadeEmpty column table then
                    count + 1

                else
                    count
        in
        -- TODO: Remove recursion?
        countEmptyCascades (column + 1) newCount table


emptyCascades : Table -> Int
emptyCascades =
    countEmptyCascades 0 0


countEmptyCells : Cell -> Int -> Table -> Int
countEmptyCells cell count table =
    if cell == table.cellsCount then
        count

    else
        let
            mCard =
                Array.get cell table.cells
                    |> Maybe.Extra.dig

            newCount =
                case mCard of
                    Just _ ->
                        count

                    Nothing ->
                        count + 1
        in
        countEmptyCells (cell + 1) newCount table


emptyCells : Table -> Int
emptyCells =
    countEmptyCells 0 0


{-| The max function takes the number of empty cascades and then the number of empty cells and returns the maximum number of cards you can move
-}
maxPileDepthAlgorithm : (Int -> Int -> Int) -> Table -> Int
maxPileDepthAlgorithm maxFn table =
    maxFn (emptyCascades table) (emptyCells table)


animateCards : Table -> Table
animateCards table =
    { table
        | diamonds = table.diamonds |> List.map Card.animate
        , clubs = table.clubs |> List.map Card.animate
        , hearts = table.hearts |> List.map Card.animate
        , spades = table.spades |> List.map Card.animate
        , cells = table.cells |> Array.map (Maybe.map Card.animate)
        , cascades = table.cascades |> Array.map (List.map Card.animate)
        , animation = StaticScene
    }


type alias InGameCard =
    { card : Card
    , cardLoc : CardLoc
    , inPile : Bool
    }


{-| Note that this method does not return cards that are "in the game" that are
in the hand of the player since those aren't tracked by the table.
-}
inGameCards : Table -> Array Row -> List InGameCard
inGameCards table cascadesPileRows =
    let
        maybeCardCollector { cardLoc, mCard } locCards =
            case mCard of
                Just card ->
                    { cardLoc = cardLoc, card = card, inPile = False } :: locCards

                Nothing ->
                    locCards

        foundations =
            [ ( StaticLoc (FoundationLoc Diamonds), table.diamonds )
            , ( StaticLoc (FoundationLoc Clubs), table.clubs )
            , ( StaticLoc (FoundationLoc Hearts), table.hearts )
            , ( StaticLoc (FoundationLoc Spades), table.spades )
            ]
                |> List.map (\( cardLoc, cards ) -> List.map (\card -> { card = card, cardLoc = cardLoc, inPile = False }) cards)
                |> List.foldl (++) []

        cells =
            table.cells
                |> Array.indexedMap (\cell mCard -> { cardLoc = StaticLoc (CellLoc cell), mCard = mCard })
                |> Array.foldl maybeCardCollector []

        locatedCascadeCards column columnCards =
            let
                cascadePileRow =
                    Array.get column cascadesPileRows
                        -- Maybe the cascade column state is paired with its pile row state or we calculate the value as needed from the cards rather than on some separate array??
                        |> Maybe.withDefault 53

                rowsCount =
                    List.length columnCards

                row inversedRow =
                    rowsCount - 1 - inversedRow
            in
            List.indexedMap
                (\inversedRow card ->
                    { cardLoc = StaticLoc (CascadeLoc column (row inversedRow))
                    , card = card
                    , inPile = row inversedRow >= cascadePileRow
                    }
                )
                columnCards

        cascades =
            table.cascades
                |> Array.indexedMap locatedCascadeCards
                |> Array.toList
                |> List.foldl (++) []
    in
    foundations ++ cells ++ cascades
