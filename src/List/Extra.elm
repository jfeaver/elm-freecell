module List.Extra exposing (find, partitionN)


partitionN : Int -> List a -> ( List a, List a )
partitionN n list =
    doPartitionN 0 ( [], list ) n


doPartitionN : Int -> ( List a, List a ) -> Int -> ( List a, List a )
doPartitionN soFar ( taken, remaining ) n =
    if soFar < n then
        case remaining of
            [] ->
                ( List.reverse taken, remaining )

            takeOne :: rest ->
                doPartitionN (soFar + 1) ( takeOne :: taken, rest ) n

    else
        ( List.reverse taken, remaining )


{-| Find the first element that satisfies a predicate and return
Just that element. If none match, return Nothing.

    find (\num -> num > 5) [ 2, 4, 6, 8 ]
    --> Just 6

-}
find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest
