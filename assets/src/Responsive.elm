module Responsive exposing (Responsive, constrainingDimension, init, square, styleList)

import Html exposing (Attribute, Html)
import Svg.Attributes exposing (style)


type Responsive
    = Responsive Width Height


type Width
    = Width Int


type Height
    = Height Int


init : { width : Int, height : Int } -> Responsive
init { width, height } =
    Responsive (Width width) (Height height)


styleList : List ( String, String ) -> Attribute msg
styleList styles =
    List.map (\( a, b ) -> a ++ ": " ++ b) styles
        |> String.join "; "
        |> style


constrainingDimension : Responsive -> Int
constrainingDimension (Responsive (Width width) (Height height)) =
    if width <= height then
        width

    else
        height


square : Responsive -> Attribute msg
square responsive =
    let
        constrainedDimension =
            constrainingDimension responsive
                |> toFloat
                |> (*) 0.5
                |> floor
                |> String.fromInt
    in
    styleList [ ( "height", constrainedDimension ++ "px" ), ( "width", constrainedDimension ++ "px" ) ]
        |> Debug.log "styles"
