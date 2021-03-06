module Chess.MetaGame exposing
    ( Callbacks
    , Model
    , Msg
    , init
    , makeGame
    , reinforcingSquares
    , subscriptions
    , update
    , view
    )

{-

   Metagame Trainer - The perspective of the General.

   It aims to feed you the tactics that you would get to practice in order to be
   a [Rated](https://en.wikipedia.org/wiki/Chess_rating_system) Chess player.


   Motivation:

   Generals have colonels and stuff that would tell them tactics (and their best council generally).
   -> And to be clear. . . if you want any real success at Chess then you will get to practice those skills as well.

   But this module is about starting to practice the meta game of Chess earlier than has previously been possible.

-}

import Browser.Dom
import Browser.Events exposing (onMouseMove)
import Chess.Logic as Logic exposing (Team)
import Chess.Position as Position exposing (Position(..))
import Dict exposing (Dict)
import Html exposing (Attribute, Html, div, fieldset, input, label, span, text)
import Html.Attributes exposing (checked, class, classList, name, type_)
import Html.Events exposing (on, onClick, onInput, onMouseDown, onMouseLeave, onMouseOver)
import Html.Lazy exposing (lazy, lazy2, lazy3, lazy4, lazy5, lazy6, lazy7)
import Json.Decode as D
import Json.Encode as E
import List.Extra
import Page.Learn.Scenario exposing (Move)
import Prelude
import Svg exposing (Svg, circle, g, svg)
import Svg.Attributes exposing (cx, cy, d, height, id, r, style, transform, version, viewBox, width, x, y)
import Task



{-
   This is the public interface for using this module.

   You need to declare these callbacks in order to hook it up.

   - makeMove -> Hook up to it a make move function that persists user actions to the backend. Feel free to wrap it.
-}


type alias Callbacks msg =
    { makeMove : Move -> msg
    , mapMsg : Msg -> msg
    }



-- MODEL


type alias Model =
    { game : Logic.Game
    , playerTeam : Team
    , reinforcing : List Position
    , opponentReinforcing : List Position
    , dragStuff : DragStuff

    -- TODO: Bleh. Do not like the direct dependency on Browser.Dom.Element
    -- Also that its a maybe.
    , browserBoard : Maybe Browser.Dom.Element

    -- responsive : Responsive
    , squareSize : Maybe Int
    }


type DragStuff
    = PieceInHand DragStuffInner
    | NoPieceInHand


type alias DragStuffInner =
    { x : Int
    , y : Int
    , piece : Logic.Piece
    , startingPosition : Position
    , dragState : DragState
    , currentPosition : Position
    }


type DragState
    = Moving Int Int Int Int


init : List Move -> String -> Maybe String -> (Msg -> msg) -> ( Model, Cmd msg )
init availableMoves currentState recentMove mapMsg =
    let
        game =
            makeGame availableMoves currentState recentMove
    in
    ( Model game Logic.Black [] [] NoPieceInHand Nothing Nothing
    , Cmd.batch
        [ Task.attempt (mapMsg << SetSquareSize) (Browser.Dom.getElement "main-board")
        , Task.attempt (mapMsg << StoreCopyOfBrowserBoard) (Browser.Dom.getElement "main-board")
        ]
    )


makeGame : List Move -> String -> Maybe String -> Logic.Game
makeGame availableMoves currentState recentMove =
    case recentMove of
        Nothing ->
            Result.withDefault Logic.blankBoard <| Logic.fromFen availableMoves currentState Nothing

        Just r ->
            case String.toList r of
                [ a, b, c, d ] ->
                    (Result.map2
                        Logic.BasicMove
                        (D.decodeValue Position.decoder (E.string (String.fromList [ a, b ])))
                        (D.decodeValue Position.decoder (E.string (String.fromList [ c, d ])))
                        |> Result.mapError (\_ -> "Not a valid move")
                    )
                        |> Result.andThen (\basicMove -> Logic.fromFen availableMoves currentState (Just basicMove))
                        |> Result.withDefault Logic.blankBoard

                _ ->
                    Result.withDefault Logic.blankBoard <| Logic.fromFen availableMoves currentState Nothing



-- Msg


type Msg
    = ChangeTeam Team
    | FindReinforcements Position ()
    | MakeMove Move
      -- Drag And Drop
    | StartDragging Int Int Position Logic.Piece
    | MoveDragging Int Int
    | StopDragging Int Int
    | MakeMoveIfDragIsOnBoard DragStuffInner (Result Browser.Dom.Error Browser.Dom.Element)
    | SetSquareSize (Result Browser.Dom.Error Browser.Dom.Element)
    | StoreCopyOfBrowserBoard (Result Browser.Dom.Error Browser.Dom.Element)



-- UPDATE


update : Callbacks msg -> Msg -> Model -> ( Model, Cmd msg )
update callbacks msg model =
    case msg of
        ChangeTeam color ->
            ( { model | playerTeam = color }, Cmd.none )

        FindReinforcements currentPosition _ ->
            case model.dragStuff of
                NoPieceInHand ->
                    ( { model | reinforcing = [], opponentReinforcing = [] }, Cmd.none )

                PieceInHand { startingPosition } ->
                    ( { model
                        | reinforcing =
                            reinforcingSquares
                                { starting = startingPosition, current = currentPosition }
                                (Logic.canAttack currentPosition model.game)
                                model.game.moves
                        , opponentReinforcing =
                            reinforcingSquares
                                { starting = startingPosition, current = currentPosition }
                                (Logic.canAttack currentPosition (Logic.nextTurn model.game |> Logic.removePiece currentPosition))
                                model.game.moves
                      }
                    , Cmd.none
                    )

        MakeMove move ->
            ( model, Task.perform callbacks.makeMove (Task.succeed move) )

        -- Drag and Drop
        StartDragging x y startingPosition piece ->
            case model.dragStuff of
                NoPieceInHand ->
                    let
                        initialDragStuff =
                            PieceInHand
                                { x = x
                                , y = y
                                , dragState = Moving x y x y
                                , startingPosition = startingPosition
                                , piece = piece
                                , currentPosition = startingPosition
                                }
                    in
                    ( { model | dragStuff = initialDragStuff, game = removePiece startingPosition model.game, opponentReinforcing = [] }
                    , Task.perform (callbacks.mapMsg << FindReinforcements startingPosition) (Task.succeed ())
                    )

                PieceInHand _ ->
                    ( model, Cmd.none )

        MoveDragging currentX currentY ->
            case model.dragStuff of
                NoPieceInHand ->
                    ( { model | reinforcing = [], opponentReinforcing = [] }, Cmd.none )

                PieceInHand ({ dragState, startingPosition, currentPosition } as dragStuffInner) ->
                    let
                        (Moving startX startY _ _) =
                            dragState

                        updatedDragState =
                            Moving startX startY currentX currentY

                        updatedDragStuffInner =
                            case model.browserBoard of
                                Nothing ->
                                    { dragStuffInner | dragState = updatedDragState }

                                Just browserBoard ->
                                    case findCurrentPosition dragState browserBoard.element model.game.turn of
                                        Nothing ->
                                            { dragStuffInner | dragState = updatedDragState }

                                        Just updatedCurrentPosition ->
                                            { dragStuffInner | dragState = updatedDragState, currentPosition = updatedCurrentPosition }
                    in
                    ( { model
                        | dragStuff = PieceInHand updatedDragStuffInner
                      }
                    , if updatedDragStuffInner.currentPosition /= currentPosition then
                        Task.perform (callbacks.mapMsg << FindReinforcements updatedDragStuffInner.currentPosition) (Task.succeed ())

                      else
                        Cmd.none
                    )

        StopDragging currentX currentY ->
            case model.dragStuff of
                NoPieceInHand ->
                    ( model, Cmd.none )

                PieceInHand ({ x, y, dragState } as dragStuffInner) ->
                    let
                        (Moving startX startY _ _) =
                            dragState

                        updatedDragStuffInner =
                            { dragStuffInner | x = x + currentX - startX, y = y + currentY - startY }
                    in
                    ( { model | dragStuff = NoPieceInHand, reinforcing = [], opponentReinforcing = [] }
                    , Task.attempt (callbacks.mapMsg << MakeMoveIfDragIsOnBoard updatedDragStuffInner) (Browser.Dom.getElement "main-board")
                    )

        MakeMoveIfDragIsOnBoard ({ piece, startingPosition } as dragStuffInner) result ->
            case result of
                Err err ->
                    ( model, Cmd.none )

                Ok boardElement ->
                    case positionOnBoard dragStuffInner boardElement.element model.game.turn of
                        Nothing ->
                            ( { model | game = placePiece startingPosition piece model.game }
                            , Cmd.none
                            )

                        Just finalPosition ->
                            case friendlyMovesToPosition model.game.turn finalPosition startingPosition (Dict.toList model.game.moves |> List.map Tuple.second) of
                                move :: [] ->
                                    ( { model | game = placePiece finalPosition piece model.game }
                                    , Task.perform callbacks.makeMove (Task.succeed move)
                                    )

                                promotionMove :: _ ->
                                    -- TODO: busted.
                                    ( { model | game = placePiece finalPosition piece model.game }
                                    , Task.perform callbacks.makeMove (Task.succeed promotionMove)
                                    )

                                [] ->
                                    ( { model | game = placePiece startingPosition piece model.game }
                                    , Cmd.none
                                    )

        -- TODO: use this instead: https://package.elm-lang.org/packages/elm/browser/latest/Browser-Events#onResize
        -- Will make all piece sizes look the same.
        -- Get the whole board element and then look it up whenever we need it and subscribe to changes.
        SetSquareSize result ->
            case result of
                Err err ->
                    ( model, Cmd.none )

                Ok boardElement ->
                    ( { model | squareSize = Just (round <| boardElement.element.width / 8) }, Cmd.none )

        StoreCopyOfBrowserBoard result ->
            case result of
                Err err ->
                    ( model, Cmd.none )

                Ok boardElement ->
                    ( { model | browserBoard = Just boardElement }, Cmd.none )



-- GAME LOGIC
{-
   Returns all squares that have pieces on them that are able to reinforce the move that is being considered.

   Will first ensure that the move is legal before finding reinforcements.
-}


reinforcingSquares : { starting : Position, current : Position } -> List Position -> Dict Logic.MoveKey Move -> List Position
reinforcingSquares { starting, current } allMovesTo legalMoves =
    if Dict.member (Position.toAlgebraic starting ++ Position.toAlgebraic current) legalMoves || starting == current then
        allMovesTo

    else
        []



-- GAME UPDATES


removePiece : Position -> Logic.Game -> Logic.Game
removePiece position game =
    Dict.remove (Position.toRaw position) game.occupiedSquares
        |> (\updatedPieces -> { game | occupiedSquares = updatedPieces })


placePiece : Position -> Logic.Piece -> Logic.Game -> Logic.Game
placePiece position piece game =
    Dict.insert (Position.toRaw position) piece game.occupiedSquares
        |> (\updatedPieces -> { game | occupiedSquares = updatedPieces })


positionOnBoard : DragStuffInner -> { x : Float, y : Float, width : Float, height : Float } -> Team -> Maybe Position
positionOnBoard { x, y, dragState } element turn =
    if positionIsOnBoard x y element then
        let
            xIntercept =
                List.Extra.find (findIntercept element.x element.width x) (List.range 1 8)
                    |> Maybe.map (\v -> 9 - v)
                    |> Maybe.map (reverseIfWhite turn)

            yIntercept =
                List.Extra.find (findIntercept element.y element.height y) (List.range 1 8)
                    |> Maybe.map (reverseIfWhite turn)
        in
        Maybe.map2 Position xIntercept yIntercept

    else
        Nothing


findIntercept : Float -> Float -> Int -> Int -> Bool
findIntercept boardStart boardSize coordinate pos =
    let
        coordinateJustified =
            coordinate - round boardStart

        squareSize =
            round <| boardSize / 8

        potentialPosition =
            pos * squareSize
    in
    coordinateJustified > (potentialPosition - squareSize) && coordinateJustified <= potentialPosition


reverseIfWhite : Team -> Int -> Int
reverseIfWhite color idx =
    if color == Logic.White then
        9 - idx

    else
        idx


findCurrentPosition : DragState -> { x : Float, y : Float, width : Float, height : Float } -> Team -> Maybe Position
findCurrentPosition dragState element turn =
    let
        (Moving _ _ currentX currentY) =
            dragState
    in
    if positionIsOnBoard currentX currentY element then
        let
            xIntercept =
                List.Extra.find (findIntercept element.x element.width currentX) (List.range 1 8)
                    |> Maybe.map (\v -> 9 - v)
                    |> Maybe.map (reverseIfWhite turn)

            yIntercept =
                List.Extra.find (findIntercept element.y element.height currentY) (List.range 1 8)
                    |> Maybe.map (reverseIfWhite turn)
        in
        Maybe.map2 Position xIntercept yIntercept

    else
        Nothing


positionIsOnBoard : Int -> Int -> { x : Float, y : Float, width : Float, height : Float } -> Bool
positionIsOnBoard x y element =
    (x >= round element.x && x <= round (element.x + element.width))
        && (y >= round element.y && y <= round (element.y + element.height))



-- VIEW BOARD


view : Model -> Html Msg
view { game, reinforcing, opponentReinforcing, playerTeam, dragStuff, squareSize } =
    div [ class "container mx-auto" ]
        [ lazy2 viewTurn game playerTeam
        , lazy6 viewBoard game reinforcing opponentReinforcing playerTeam dragStuff squareSize
        , lazy viewSettings playerTeam
        ]


viewSettings : Team -> Html Msg
viewSettings color =
    div []
        [ fieldset []
            [ radio "Black" (color == Logic.Black) (\_ -> ChangeTeam Logic.Black)
            , radio "White" (color == Logic.White) (\_ -> ChangeTeam Logic.White)
            ]
        ]


radio : String -> Bool -> (String -> Msg) -> Html Msg
radio value isChecked msg =
    label
        []
        [ input [ type_ "radio", name "Team", onInput msg, checked isChecked ] []
        , text value
        ]


viewTurn : Logic.Game -> Team -> Html Msg
viewTurn game playerColor =
    div [ class "container mx-auto text-2xl" ]
        [ if game.turn == playerColor then
            text "Your turn"

          else
            text "Opponents turn"
        ]


viewBoard : Logic.Game -> List Position -> List Position -> Team -> DragStuff -> Maybe Int -> Html Msg
viewBoard game reinforcing opponentReinforcing playerColor dragStuff squareSize =
    div []
        [ Prelude.maybe (span [] []) (lazy4 viewGhostSquare game playerColor dragStuff) squareSize
        , div
            [ id "main-board"
            , classList
                [ ( "h-96 w-96 md:h-constrained-1/2 md:w-constrained-1/2 lg:h-constrained-40% lg:w-constrained-40% 2xl:h-constrained-40% 2xl:w-constrained-40% grid grid-cols-8 grid-rows-8 border-2 border-gray-500 gap-0 shadow-2xl", True )
                , ( "rotated", playerColor == Logic.White )
                ]
            ]
            (List.concatMap (viewRow game reinforcing opponentReinforcing playerColor) (List.range 1 8))
        ]


viewRow : Logic.Game -> List Position -> List Position -> Team -> Int -> List (Html Msg)
viewRow game reinforcing opponentReinforcing playerColor row =
    List.map (lazy6 viewCell game reinforcing opponentReinforcing playerColor row) (List.reverse (List.range 1 8))


viewCell : Logic.Game -> List Position -> List Position -> Team -> Int -> Int -> Html Msg
viewCell game reinforcing opponentReinforcing playerColor row column =
    div
        [ classList
            [ ( "column-" ++ String.fromInt column, True )
            , ( "row-" ++ String.fromInt row, True )
            , ( shading column row, True )
            , ( "rotated", playerColor == Logic.White )
            ]
        ]
        [ lazy5 viewSquare game reinforcing opponentReinforcing playerColor (Position column row) ]


viewGhostSquare : Logic.Game -> Team -> DragStuff -> Int -> Html Msg
viewGhostSquare game playerColor dragStuff squareSize =
    case dragStuff of
        NoPieceInHand ->
            span [] []

        PieceInHand ({ piece } as dragStuffInner) ->
            let
                ( x, y ) =
                    getDragCoordinates dragStuffInner
            in
            div
                [ classList
                    [ ( "bg-transparent", True )
                    , ( "grid-cols-none", True )
                    , ( "absolute", True )
                    , ( "z-50", True )
                    , ( "h-12 w-12 md:h-16 md:w-16 lg:h-20 lg:w-20 2xl:h-28 2xl:w-28", True )
                    ]
                , styleList
                    [ "left: " ++ String.fromInt (x - (squareSize // 2)) ++ "px"
                    , "top: " ++ String.fromInt (y - (squareSize // 2)) ++ "px"
                    , "cursor: grabbing"
                    ]
                , on "mouseup" (D.map2 StopDragging pageX pageY)
                ]
                [ findSvg piece [] ]


styleList : List String -> Attribute Msg
styleList styles =
    String.join "; " styles
        |> style


getDragCoordinates : DragStuffInner -> ( Int, Int )
getDragCoordinates { x, y, dragState } =
    let
        (Moving startX startY endX endY) =
            dragState
    in
    ( x + endX - startX, y + endY - startY )


viewSquare : Logic.Game -> List Position -> List Position -> Team -> Position -> Html Msg
viewSquare game reinforcing opponentReinforcing playerColor position =
    if game.turn == playerColor then
        case Dict.get (Position.toRaw position) game.occupiedSquares of
            Just piece ->
                div
                    [ classList
                        [ ( "w-full h-full m-0", True )
                        , ( "bg-green-500", reinforces position reinforcing )
                        , ( "bg-yellow-500", reinforces position opponentReinforcing )
                        , ( "bg-gray-400 bg-opacity-50 border-4 border-black", isRecentMove position game.recentMove )
                        , ( "transition", True )
                        , ( Position.toAlgebraic position, True )
                        ]
                    , style "cursor: grab"
                    , on "mousedown" (D.map4 StartDragging pageX pageY (D.succeed position) (D.succeed piece))
                    ]
                    [ findSvg piece [] ]

            Nothing ->
                div
                    [ classList
                        [ ( "w-full h-full m-0", True )
                        , ( "bg-gray-400 bg-opacity-50 border-4 border-black", isRecentMove position game.recentMove )
                        ]
                    ]
                    []

    else
        viewSquareDetails game.occupiedSquares position [] []


isRecentMove : Position -> Maybe Logic.BasicMove -> Bool
isRecentMove position recentMove =
    Prelude.maybe False (\{ from, to } -> from == position || to == position) recentMove


reinforces : Position -> List Position -> Bool
reinforces position reinforcing =
    List.any ((==) position) reinforcing


viewSquareDetails : Dict ( Int, Int ) Logic.Piece -> Position -> List ( String, Bool ) -> List (Attribute Msg) -> Html Msg
viewSquareDetails pieces position additionalClasses additionalEvents =
    div
        ([ classList
            ([ ( "w-full h-full m-0", True )
             ]
                ++ additionalClasses
            )
         ]
            ++ additionalEvents
        )
        (case Dict.get (Position.toRaw position) pieces of
            Just piece ->
                [ findSvg piece [] ]

            Nothing ->
                []
        )


shading : Int -> Int -> String
shading column row =
    if modBy 2 (column + row) == 0 then
        "bg-gray-200"

    else
        "bg-indigo-400"



-- SQUARE PROPERTIES


friendlyMovesToPosition : Team -> Position -> Position -> List Move -> List Move
friendlyMovesToPosition turn squareTo squareFrom moves =
    movesToPosition squareTo squareFrom moves
        |> List.filter (\move -> move.color == turnToColor turn)


friendlyMoves : Team -> Move -> Bool
friendlyMoves turn move =
    move.color == turnToColor turn


movesToSquare : Position -> Move -> Bool
movesToSquare squareTo move =
    move.squareTo == Position.toAlgebraic squareTo



-- TODO: clean this up with a custom codec.


turnToColor : Team -> String
turnToColor color =
    case color of
        Logic.Black ->
            "b"

        Logic.White ->
            "w"


movesToPosition : Position -> Position -> List Move -> List Move
movesToPosition squareTo squareFrom =
    List.filter (\move -> move.squareFrom == Position.toAlgebraic squareFrom && move.squareTo == Position.toAlgebraic squareTo)



-- SVG


{-| -}
findSvg : Logic.Piece -> List (Html.Attribute msg) -> Html msg
findSvg (Logic.Piece color pieceType) =
    case ( color, pieceType ) of
        ( Logic.White, Logic.Pawn ) ->
            whitePawn

        ( Logic.Black, Logic.Pawn ) ->
            blackPawn

        ( Logic.White, Logic.Bishop ) ->
            whiteBishop

        ( Logic.Black, Logic.Bishop ) ->
            blackBishop

        ( Logic.White, Logic.Knight ) ->
            whiteKnight

        ( Logic.Black, Logic.Knight ) ->
            blackKnight

        ( Logic.White, Logic.Rook ) ->
            whiteRook

        ( Logic.Black, Logic.Rook ) ->
            blackRook

        ( Logic.White, Logic.Advisor ) ->
            whiteAdvisor

        ( Logic.Black, Logic.Advisor ) ->
            blackAdvisor

        ( Logic.White, Logic.Monarch ) ->
            whiteMonarch

        ( Logic.Black, Logic.Monarch ) ->
            blackMonarch


blackPawn : List (Html.Attribute msg) -> Html msg
blackPawn extraArguments =
    svgWithViewPort [ Svg.path [ d "m 22.5,9 c -2.21,0 -4,1.79 -4,4 0,0.89 0.29,1.71 0.78,2.38 C 17.33,16.5 16,18.59 16,21 c 0,2.03 0.94,3.84 2.41,5.03 C 15.41,27.09 11,31.58 11,39.5 H 34 C 34,31.58 29.59,27.09 26.59,26.03 28.06,24.84 29,23.03 29,21 29,18.59 27.67,16.5 25.72,15.38 26.21,14.71 26.5,13.89 26.5,13 c 0,-2.21 -1.79,-4 -4,-4 z", style "opacity:1; fill:#000000; fill-opacity:1; fill-rule:nonzero; stroke:#000000; stroke-width:1.5; stroke-linecap:round; stroke-linejoin:miter; stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [] ]


whitePawn : List (Html.Attribute msg) -> Html msg
whitePawn extraAttributes =
    svgWithViewPort [ Svg.path [ d "m 22.5,9 c -2.21,0 -4,1.79 -4,4 0,0.89 0.29,1.71 0.78,2.38 C 17.33,16.5 16,18.59 16,21 c 0,2.03 0.94,3.84 2.41,5.03 C 15.41,27.09 11,31.58 11,39.5 H 34 C 34,31.58 29.59,27.09 26.59,26.03 28.06,24.84 29,23.03 29,21 29,18.59 27.67,16.5 25.72,15.38 26.21,14.71 26.5,13.89 26.5,13 c 0,-2.21 -1.79,-4 -4,-4 z", style "opacity:1; fill:#ffffff; fill-opacity:1; fill-rule:nonzero; stroke:#000000; stroke-width:1.5; stroke-linecap:round; stroke-linejoin:miter; stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [] ]


blackBishop : List (Html.Attribute msg) -> Html msg
blackBishop extraAttributes =
    svgWithViewPort [ g [ style "opacity:1; fill:none; fill-rule:evenodd; fill-opacity:1; stroke:#000000; stroke-width:1.5; stroke-linecap:round; stroke-linejoin:round; stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ g [ style "fill:#000000; stroke:#000000; stroke-linecap:butt;" ] [ Svg.path [ d "M 9,36 C 12.39,35.03 19.11,36.43 22.5,34 C 25.89,36.43 32.61,35.03 36,36 C 36,36 37.65,36.54 39,38 C 38.32,38.97 37.35,38.99 36,38.5 C 32.61,37.53 25.89,38.96 22.5,37.5 C 19.11,38.96 12.39,37.53 9,38.5 C 7.65,38.99 6.68,38.97 6,38 C 7.35,36.54 9,36 9,36 z" ] [], Svg.path [ d "M 15,32 C 17.5,34.5 27.5,34.5 30,32 C 30.5,30.5 30,30 30,30 C 30,27.5 27.5,26 27.5,26 C 33,24.5 33.5,14.5 22.5,10.5 C 11.5,14.5 12,24.5 17.5,26 C 17.5,26 15,27.5 15,30 C 15,30 14.5,30.5 15,32 z" ] [], Svg.path [ d "M 25 8 A 2.5 2.5 0 1 1 20,8 A 2.5 2.5 0 1 1 25 8 z" ] [] ], Svg.path [ d "M 17.5,26 L 27.5,26 M 15,30 L 30,30 M 22.5,15.5 L 22.5,20.5 M 20,18 L 25,18", style "fill:none; stroke:#ffffff; stroke-linejoin:miter;" ] [] ] ]


whiteBishop : List (Html.Attribute msg) -> Html msg
whiteBishop extraAttributes =
    svgWithViewPort [ g [ style "opacity:1; fill:none; fill-rule:evenodd; fill-opacity:1; stroke:#000000; stroke-width:1.5; stroke-linecap:round; stroke-linejoin:round; stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ g [ style "fill:#ffffff; stroke:#000000; stroke-linecap:butt;" ] [ Svg.path [ d "M 9,36 C 12.39,35.03 19.11,36.43 22.5,34 C 25.89,36.43 32.61,35.03 36,36 C 36,36 37.65,36.54 39,38 C 38.32,38.97 37.35,38.99 36,38.5 C 32.61,37.53 25.89,38.96 22.5,37.5 C 19.11,38.96 12.39,37.53 9,38.5 C 7.65,38.99 6.68,38.97 6,38 C 7.35,36.54 9,36 9,36 z" ] [], Svg.path [ d "M 15,32 C 17.5,34.5 27.5,34.5 30,32 C 30.5,30.5 30,30 30,30 C 30,27.5 27.5,26 27.5,26 C 33,24.5 33.5,14.5 22.5,10.5 C 11.5,14.5 12,24.5 17.5,26 C 17.5,26 15,27.5 15,30 C 15,30 14.5,30.5 15,32 z" ] [], Svg.path [ d "M 25 8 A 2.5 2.5 0 1 1 20,8 A 2.5 2.5 0 1 1 25 8 z" ] [] ], Svg.path [ d "M 17.5,26 L 27.5,26 M 15,30 L 30,30 M 22.5,15.5 L 22.5,20.5 M 20,18 L 25,18", style "fill:none; stroke:#000000; stroke-linejoin:miter;" ] [] ] ]


blackKnight : List (Html.Attribute msg) -> Html msg
blackKnight extraAttributes =
    svgWithViewPort [ g [ style "opacity:1; fill:none; fill-opacity:1; fill-rule:evenodd; stroke:#000000; stroke-width:1.5; stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ Svg.path [ d "M 22,10 C 32.5,11 38.5,18 38,39 L 15,39 C 15,30 25,32.5 23,18", style "fill:#000000; stroke:#000000;" ] [], Svg.path [ d "M 24,18 C 24.38,20.91 18.45,25.37 16,27 C 13,29 13.18,31.34 11,31 C 9.958,30.06 12.41,27.96 11,28 C 10,28 11.19,29.23 10,30 C 9,30 5.997,31 6,26 C 6,24 12,14 12,14 C 12,14 13.89,12.1 14,10.5 C 13.27,9.506 13.5,8.5 13.5,7.5 C 14.5,6.5 16.5,10 16.5,10 L 18.5,10 C 18.5,10 19.28,8.008 21,7 C 22,7 22,10 22,10", style "fill:#000000; stroke:#000000;" ] [], Svg.path [ d "M 9.5 25.5 A 0.5 0.5 0 1 1 8.5,25.5 A 0.5 0.5 0 1 1 9.5 25.5 z", style "fill:#ffffff; stroke:#ffffff;" ] [], Svg.path [ d "M 15 15.5 A 0.5 1.5 0 1 1 14,15.5 A 0.5 1.5 0 1 1 15 15.5 z", transform "matrix(0.866,0.5,-0.5,0.866,9.693,-5.173)", style "fill:#ffffff; stroke:#ffffff;" ] [], Svg.path [ d "M 24.55,10.4 L 24.1,11.85 L 24.6,12 C 27.75,13 30.25,14.49 32.5,18.75 C 34.75,23.01 35.75,29.06 35.25,39 L 35.2,39.5 L 37.45,39.5 L 37.5,39 C 38,28.94 36.62,22.15 34.25,17.66 C 31.88,13.17 28.46,11.02 25.06,10.5 L 24.55,10.4 z ", style "fill:#ffffff; stroke:none;" ] [] ] ]


whiteKnight : List (Html.Attribute msg) -> Html msg
whiteKnight extraAttributes =
    svgWithViewPort [ g [ style "opacity:1; fill:none; fill-opacity:1; fill-rule:evenodd; stroke:#000000; stroke-width:1.5; stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ Svg.path [ d "M 22,10 C 32.5,11 38.5,18 38,39 L 15,39 C 15,30 25,32.5 23,18", style "fill:#ffffff; stroke:#000000;" ] [], Svg.path [ d "M 24,18 C 24.38,20.91 18.45,25.37 16,27 C 13,29 13.18,31.34 11,31 C 9.958,30.06 12.41,27.96 11,28 C 10,28 11.19,29.23 10,30 C 9,30 5.997,31 6,26 C 6,24 12,14 12,14 C 12,14 13.89,12.1 14,10.5 C 13.27,9.506 13.5,8.5 13.5,7.5 C 14.5,6.5 16.5,10 16.5,10 L 18.5,10 C 18.5,10 19.28,8.008 21,7 C 22,7 22,10 22,10", style "fill:#ffffff; stroke:#000000;" ] [], Svg.path [ d "M 9.5 25.5 A 0.5 0.5 0 1 1 8.5,25.5 A 0.5 0.5 0 1 1 9.5 25.5 z", style "fill:#000000; stroke:#000000;" ] [], Svg.path [ d "M 15 15.5 A 0.5 1.5 0 1 1 14,15.5 A 0.5 1.5 0 1 1 15 15.5 z", transform "matrix(0.866,0.5,-0.5,0.866,9.693,-5.173)", style "fill:#000000; stroke:#000000;" ] [] ] ]


blackRook : List (Html.Attribute msg) -> Html msg
blackRook extraAttributes =
    svgWithViewPort [ g [ style "opacity:1; fill:000000; fill-opacity:1; fill-rule:evenodd; stroke:#000000; stroke-width:1.5; stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ Svg.path [ x "0", y "0", width "100%", height "100%", d "M 9,39 L 36,39 L 36,36 L 9,36 L 9,39 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 12.5,32 L 14,29.5 L 31,29.5 L 32.5,32 L 12.5,32 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 12,36 L 12,32 L 33,32 L 33,36 L 12,36 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 14,29.5 L 14,16.5 L 31,16.5 L 31,29.5 L 14,29.5 z ", style "stroke-linecap:butt;stroke-linejoin:miter;" ] [], Svg.path [ d "M 14,16.5 L 11,14 L 34,14 L 31,16.5 L 14,16.5 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 11,14 L 11,9 L 15,9 L 15,11 L 20,11 L 20,9 L 25,9 L 25,11 L 30,11 L 30,9 L 34,9 L 34,14 L 11,14 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 12,35.5 L 33,35.5 L 33,35.5", style "fill:none; stroke:#ffffff; stroke-width:1; stroke-linejoin:miter;" ] [], Svg.path [ d "M 13,31.5 L 32,31.5", style "fill:none; stroke:#ffffff; stroke-width:1; stroke-linejoin:miter;" ] [], Svg.path [ d "M 14,29.5 L 31,29.5", style "fill:none; stroke:#ffffff; stroke-width:1; stroke-linejoin:miter;" ] [], Svg.path [ d "M 14,16.5 L 31,16.5", style "fill:none; stroke:#ffffff; stroke-width:1; stroke-linejoin:miter;" ] [], Svg.path [ d "M 11,14 L 34,14", style "fill:none; stroke:#ffffff; stroke-width:1; stroke-linejoin:miter;" ] [] ] ]


whiteRook : List (Html.Attribute msg) -> Html msg
whiteRook extraAttributes =
    svgWithViewPort [ g [ style "opacity:1; fill:#ffffff; fill-opacity:1; fill-rule:evenodd; stroke:#000000; stroke-width:1.5; stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ Svg.path [ d "M 9,39 L 36,39 L 36,36 L 9,36 L 9,39 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 12,36 L 12,32 L 33,32 L 33,36 L 12,36 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 11,14 L 11,9 L 15,9 L 15,11 L 20,11 L 20,9 L 25,9 L 25,11 L 30,11 L 30,9 L 34,9 L 34,14", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 34,14 L 31,17 L 14,17 L 11,14" ] [], Svg.path [ d "M 31,17 L 31,29.5 L 14,29.5 L 14,17", style "stroke-linecap:butt; stroke-linejoin:miter;" ] [], Svg.path [ d "M 31,29.5 L 32.5,32 L 12.5,32 L 14,29.5" ] [], Svg.path [ d "M 11,14 L 34,14", style "fill:none; stroke:#000000; stroke-linejoin:miter;" ] [] ] ]


blackAdvisor : List (Html.Attribute msg) -> Html msg
blackAdvisor extraAttributes =
    svgWithViewPort [ g [ style "opacity:1; fill:000000; fill-opacity:1; fill-rule:evenodd; stroke:#000000; stroke-width:1.5; stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ g [ style "fill:#000000; stroke:none;" ] [ circle [ cx "6", cy "12", r "2.75" ] [], circle [ cx "14", cy "9", r "2.75" ] [], circle [ cx "22.5", cy "8", r "2.75" ] [], circle [ cx "31", cy "9", r "2.75" ] [], circle [ cx "39", cy "12", r "2.75" ] [] ], Svg.path [ d "M 9,26 C 17.5,24.5 30,24.5 36,26 L 38.5,13.5 L 31,25 L 30.7,10.9 L 25.5,24.5 L 22.5,10 L 19.5,24.5 L 14.3,10.9 L 14,25 L 6.5,13.5 L 9,26 z", style "stroke-linecap:butt; stroke:#000000;" ] [], Svg.path [ d "M 9,26 C 9,28 10.5,28 11.5,30 C 12.5,31.5 12.5,31 12,33.5 C 10.5,34.5 11,36 11,36 C 9.5,37.5 11,38.5 11,38.5 C 17.5,39.5 27.5,39.5 34,38.5 C 34,38.5 35.5,37.5 34,36 C 34,36 34.5,34.5 33,33.5 C 32.5,31 32.5,31.5 33.5,30 C 34.5,28 36,28 36,26 C 27.5,24.5 17.5,24.5 9,26 z", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 11,38.5 A 35,35 1 0 0 34,38.5", style "fill:none; stroke:#000000; stroke-linecap:butt;" ] [], Svg.path [ d "M 11,29 A 35,35 1 0 1 34,29", style "fill:none; stroke:#ffffff;" ] [], Svg.path [ d "M 12.5,31.5 L 32.5,31.5", style "fill:none; stroke:#ffffff;" ] [], Svg.path [ d "M 11.5,34.5 A 35,35 1 0 0 33.5,34.5", style "fill:none; stroke:#ffffff;" ] [], Svg.path [ d "M 10.5,37.5 A 35,35 1 0 0 34.5,37.5", style "fill:none; stroke:#ffffff;" ] [] ] ]


whiteAdvisor : List (Html.Attribute msg) -> Html msg
whiteAdvisor extraAttributes =
    svgWithViewPort [ g [ style "opacity:1; fill:#ffffff; fill-opacity:1; fill-rule:evenodd; stroke:#000000; stroke-width:1.5; stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ Svg.path [ d "M 9 13 A 2 2 0 1 1 5,13 A 2 2 0 1 1 9 13 z", transform "translate(-1,-1)" ] [], Svg.path [ d "M 9 13 A 2 2 0 1 1 5,13 A 2 2 0 1 1 9 13 z", transform "translate(15.5,-5.5)" ] [], Svg.path [ d "M 9 13 A 2 2 0 1 1 5,13 A 2 2 0 1 1 9 13 z", transform "translate(32,-1)" ] [], Svg.path [ d "M 9 13 A 2 2 0 1 1 5,13 A 2 2 0 1 1 9 13 z", transform "translate(7,-4)" ] [], Svg.path [ d "M 9 13 A 2 2 0 1 1 5,13 A 2 2 0 1 1 9 13 z", transform "translate(24,-4)" ] [], Svg.path [ d "M 9,26 C 17.5,24.5 27.5,24.5 36,26 L 38,14 L 31,25 L 31,11 L 25.5,24.5 L 22.5,9.5 L 19.5,24.5 L 14,11 L 14,25 L 7,14 L 9,26 z ", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 9,26 C 9,28 10.5,28 11.5,30 C 12.5,31.5 12.5,31 12,33.5 C 10.5,34.5 11,36 11,36 C 9.5,37.5 11,38.5 11,38.5 C 17.5,39.5 27.5,39.5 34,38.5 C 34,38.5 35.5,37.5 34,36 C 34,36 34.5,34.5 33,33.5 C 32.5,31 32.5,31.5 33.5,30 C 34.5,28 36,28 36,26 C 27.5,24.5 17.5,24.5 9,26 z", style "stroke-linecap:butt;" ] [], Svg.path [ d "M 11.5,30 C 15,29 30,29 33.5,30", style "fill:none;" ] [], Svg.path [ d "M 12,33.5 C 18,32.5 27,32.5 33,33.5", style "fill:none;" ] [] ] ]


blackMonarch : List (Html.Attribute msg) -> Html msg
blackMonarch extraAttributes =
    svgWithViewPort [ g [ style "fill:none; fill-opacity:1; fill-rule:evenodd; stroke:#000000; stroke-width:1.5; stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ Svg.path [ d "M 22.5,11.63 L 22.5,6", style "fill:none; stroke:#000000; stroke-linejoin:miter;", id "path6570" ] [], Svg.path [ d "M 22.5,25 C 22.5,25 27,17.5 25.5,14.5 C 25.5,14.5 24.5,12 22.5,12 C 20.5,12 19.5,14.5 19.5,14.5 C 18,17.5 22.5,25 22.5,25", style "fill:#000000;fill-opacity:1; stroke-linecap:butt; stroke-linejoin:miter;" ] [], Svg.path [ d "M 12.5,37 C 18,40.5 27,40.5 32.5,37 L 32.5,30 C 32.5,30 41.5,25.5 38.5,19.5 C 34.5,13 25,16 22.5,23.5 L 22.5,27 L 22.5,23.5 C 20,16 10.5,13 6.5,19.5 C 3.5,25.5 12.5,30 12.5,30 L 12.5,37", style "fill:#000000; stroke:#000000;" ] [], Svg.path [ d "M 20,8 L 25,8", style "fill:none; stroke:#000000; stroke-linejoin:miter;" ] [], Svg.path [ d "M 32,29.5 C 32,29.5 40.5,25.5 38.03,19.85 C 34.15,14 25,18 22.5,24.5 L 22.5,26.6 L 22.5,24.5 C 20,18 10.85,14 6.97,19.85 C 4.5,25.5 13,29.5 13,29.5", style "fill:none; stroke:#ffffff;" ] [], Svg.path [ d "M 12.5,30 C 18,27 27,27 32.5,30 M 12.5,33.5 C 18,30.5 27,30.5 32.5,33.5 M 12.5,37 C 18,34 27,34 32.5,37", style "fill:none; stroke:#ffffff;" ] [] ] ]


whiteMonarch : List (Html.Attribute msg) -> Html msg
whiteMonarch extraAttributes =
    svgWithViewPort [ g [ style "fill:none; fill-opacity:1; fill-rule:evenodd; stroke:#000000; stroke-width:1.5; stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:4; stroke-dasharray:none; stroke-opacity:1;" ] [ Svg.path [ d "M 22.5,11.63 L 22.5,6", style "fill:none; stroke:#000000; stroke-linejoin:miter;" ] [], Svg.path [ d "M 20,8 L 25,8", style "fill:none; stroke:#000000; stroke-linejoin:miter;" ] [], Svg.path [ d "M 22.5,25 C 22.5,25 27,17.5 25.5,14.5 C 25.5,14.5 24.5,12 22.5,12 C 20.5,12 19.5,14.5 19.5,14.5 C 18,17.5 22.5,25 22.5,25", style "fill:#ffffff; stroke:#000000; stroke-linecap:butt; stroke-linejoin:miter;" ] [], Svg.path [ d "M 12.5,37 C 18,40.5 27,40.5 32.5,37 L 32.5,30 C 32.5,30 41.5,25.5 38.5,19.5 C 34.5,13 25,16 22.5,23.5 L 22.5,27 L 22.5,23.5 C 20,16 10.5,13 6.5,19.5 C 3.5,25.5 12.5,30 12.5,30 L 12.5,37", style "fill:#ffffff; stroke:#000000;" ] [], Svg.path [ d "M 12.5,30 C 18,27 27,27 32.5,30", style "fill:none; stroke:#000000;" ] [], Svg.path [ d "M 12.5,33.5 C 18,30.5 27,30.5 32.5,33.5", style "fill:none; stroke:#000000;" ] [], Svg.path [ d "M 12.5,37 C 18,34 27,34 32.5,37", style "fill:none; stroke:#000000;" ] [] ] ]


svgWithViewPort : List (Svg msg) -> Html msg
svgWithViewPort =
    svg [ viewBox "-6 -6 60 60", version "1.1", width "100%", height "100%" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragStuff of
        NoPieceInHand ->
            Sub.none

        PieceInHand _ ->
            onMouseMove (D.map2 MoveDragging pageX pageY)


pageX : D.Decoder Int
pageX =
    D.field "pageX" D.int


pageY : D.Decoder Int
pageY =
    D.field "pageY" D.int
