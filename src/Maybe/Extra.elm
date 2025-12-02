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
