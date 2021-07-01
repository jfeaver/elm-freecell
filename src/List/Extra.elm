module List.Extra exposing (partitionN)


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
