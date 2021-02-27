module Structures.DirectionalList exposing (DirectionalList, fromList, selected, toEnd)

{-| Keeps track of progress through a linked list.

directionalList [1, 2, 3, 4, 5, 6]
-> myDirectionalList -- Start [1, 2, 3, 4, 5, 6]

forward myDirectionalList
-> Middle [1] 2 [3, 4, 5, 6]

forward <| forward myDirectionalList
-> Middle [2, 1] 3 [4, 5, 6]
This is done so that we are always doing stuff with the head of the list.

-}


type DirectionalList a
    = Start (List a)
    | Middle (List a) a (List a)
    | End (List a) a


fromList : List a -> DirectionalList a
fromList a =
    Start a


toEnd : DirectionalList a -> DirectionalList a
toEnd directional =
    case directional of
        Start ll ->
            case List.reverse ll of
                h :: rem ->
                    End rem h

                _ ->
                    Start ll

        Middle lookedAt current rem ->
            case List.reverse rem of
                h :: remm ->
                    End (remm ++ [ current ] ++ lookedAt) h

                _ ->
                    End lookedAt current

        End _ _ ->
            directional


selected : DirectionalList a -> Maybe a
selected directional =
    case directional of
        Start _ ->
            Nothing

        Middle previous current remaining ->
            Just current

        End previous current ->
            Just current
