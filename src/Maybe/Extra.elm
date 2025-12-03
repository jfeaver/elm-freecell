module Maybe.Extra exposing (..)


isJust : Maybe a -> Bool
isJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


isNothing : Maybe a -> Bool
isNothing =
    isJust >> not


dig : Maybe (Maybe a) -> Maybe a
dig maybeMaybe =
    case maybeMaybe of
        Just maybe ->
            maybe

        Nothing ->
            Nothing


or : Maybe a -> Maybe a -> Maybe a
or maybeA maybeB =
    case maybeA of
        Just a ->
            Just a

        Nothing ->
            maybeB


{-| Try two tests and return the first one that doesn't return Nothing.

    validMonth month =
        if month >= 1 && month <= 12 then
            Just "month"

        else
            Nothing

    validYear year =
        if year >= 2000 && year <= 2099 then
            Just "year"

        else
            Nothing

    try validMonth validYear 6
    --> Just "month"
    try validMonth validYear 2025
    --> Just "year"
    try validMonth validYear 0
    --> Nothing

-}
try : (a -> Maybe b) -> (a -> Maybe b) -> a -> Maybe b
try firstTrial secondTrial thing =
    case firstTrial thing of
        Just something ->
            Just something

        Nothing ->
            secondTrial thing


try3 : (a -> Maybe b) -> (a -> Maybe b) -> (a -> Maybe b) -> a -> Maybe b
try3 firstTrial secondTrial thirdTrial =
    try (try firstTrial secondTrial) thirdTrial


try4 : (a -> Maybe b) -> (a -> Maybe b) -> (a -> Maybe b) -> (a -> Maybe b) -> a -> Maybe b
try4 firstTrial secondTrial thirdTrial fourthTrial =
    try (try3 firstTrial secondTrial thirdTrial) fourthTrial
