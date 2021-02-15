module Prelude exposing (Segment(..), dataId, maybe)

import Html exposing (Attribute)
import Html.Attributes exposing (attribute)


type Segment
    = Text String
    | Link String String


maybe : b -> (a -> b) -> Maybe a -> b
maybe default f x =
    case x of
        Nothing ->
            default

        Just a ->
            f a


dataId : String -> String -> Attribute msg
dataId tag value =
    attribute ("data-" ++ tag) value
