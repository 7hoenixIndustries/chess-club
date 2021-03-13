module Chess.MetaGame exposing
    ( Callbacks
    , Model
    , Msg
    , init
    , makeGame
    , moveMade
    , reinforcingSquares
    , subscriptions
    , update
    , view
    )

{-| Metagame Trainer - The perspective of the General.

It aims to feed you the tactics that you would get to practice in order to be
a [Rated](https://en.wikipedia.org/wiki/Chess_rating_system) Chess player.

Motivation:

Generals have colonels and stuff that would tell them tactics (and their best council generally).
-> And to be clear. . . if you want any real success at Chess then you will get to practice those skills as well.

But this module is about starting to practice the meta game of Chess earlier than has previously been possible.

-}

import Browser.Dom
import Browser.Events exposing (onMouseMove)
import Chess.Logic as Logic exposing (Piece, Team)
import Chess.Position as Position exposing (Position(..))
import Dict exposing (Dict)
import Html exposing (Attribute, Html, a, button, div, fieldset, h1, h3, img, input, label, li, main_, nav, p, span, text, time, ul)
import Html.Attributes as Attr exposing (checked, class, classList, name, type_)
import Html.Events exposing (on, onClick, onInput, onMouseDown, onMouseLeave, onMouseOver)
import Html.Lazy exposing (lazy, lazy2, lazy3, lazy4, lazy5, lazy6, lazy7, lazy8)
import Json.Decode as D
import List.Extra
import Page.Learn.Scenario exposing (BasicMove(..), Fen(..), Move, MoveCommand, PreviousMovesSafe(..))
import Prelude
import Process
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Svg exposing (Svg, circle, g, path, svg)
import Svg.Attributes as SvgAttr exposing (cx, cy, d, height, id, r, style, transform, version, viewBox, width, x, y, z)
import Task exposing (Task)


{-| Defined Callbacks [Interface]

This is the public interface for using this module.

You need to declare these callbacks in order to hook it up.

  - makeMove -> Hook up to it a make move function that persists user actions to the backend. Feel free to wrap it.
  - mapMsg -> Pass in a function that will allow us to accept our own messages. It's probably the Msg that you would Cmd.map MyMetaGameMsg Msg.
    It's mostly used for animations.

-}
type alias Callbacks msg =
    { makeMove : Move -> msg
    , mapMsg : Msg -> msg
    }



-- MODEL


type alias Model =
    { game : AnimatedGame
    , movesHistorical : Historical
    , playerTeam : Team
    , reinforcing : List Position
    , opponentReinforcing : List Position
    , dragStuff : DragStuff
    , previousMove : Maybe BasicMoveWithPieces

    -- TODO: Bleh. Do not like the direct dependency on Browser.Dom.Element
    -- Also that its a maybe.
    , browserBoard : Maybe Browser.Dom.Element
    , mainNav : Maybe Browser.Dom.Element
    , madeMoveRecently : Bool
    , openingState : Fen

    -- responsive : Responsive
    }


type AnimatedGame
    = Animating { before : Logic.Game, after : Logic.Game }
    | DoneAnimating Logic.Game



-- STEPS to make this work with the simple animation library
-- [X] "normal moves" on init. Remove piece from board and show it.
-- [X] When picking up a piece and then putting it back. . . have it return with the updated board (not the animation one)
-- [X] captures: if piece in 'to' square. . . have it fade out opacity.
-- [X] FIX turn post animation.
-- [X] FIX background color on animation.
-- [X] IF you made the move do not show the animation when moveMade is called.
-- [X] IF you didn't, then do.
-- [X] FIX changeTeam to redo animation
-- [] Add click handler for animation piece
-- [X] Castling. . . find both and handle.
-- [] en passant . . . remove with opacity


type Historical
    = Historical (List ( BasicMove, Fen ))


type BasicMoveWithPieces
    = BasicMoveWithPieces { from : Position, to : Position, pieceMoved : Logic.Piece, pieceCaptured : Maybe Logic.Piece }
    | CastlingMove { monarchFrom : Position, monarchTo : Position, team : Logic.Team, rookFrom : Position, rookTo : Position }


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


init : List Move -> Fen -> PreviousMovesSafe -> (Msg -> msg) -> ( Model, Cmd msg )
init availableMoves currentState (PreviousMovesSafe pm) mapMsg =
    let
        previousMoves =
            List.reverse pm

        ( game, basicMoveWithPieces ) =
            case previousMoves of
                [] ->
                    -- Opening Move
                    ( DoneAnimating <| makeGame availableMoves currentState, Nothing )

                ( basicMove, fenAfterMove ) :: [] ->
                    case makeGameForAnimating availableMoves currentState basicMove of
                        Ok ( g, bMWP ) ->
                            ( Animating { before = g, after = makeGame availableMoves fenAfterMove }
                            , Just bMWP
                            )

                        Err _ ->
                            ( DoneAnimating Logic.blankBoard, Nothing )

                ( basicMove, fenAfterMove ) :: ( _, currentFen ) :: _ ->
                    case makeGameForAnimating availableMoves currentFen basicMove of
                        Ok ( g, bMWP ) ->
                            ( Animating { before = g, after = makeGame availableMoves fenAfterMove }
                            , Just bMWP
                            )

                        Err _ ->
                            ( DoneAnimating Logic.blankBoard, Nothing )
    in
    ( Model game (Historical previousMoves) Logic.Black [] [] NoPieceInHand basicMoveWithPieces Nothing Nothing False currentState
    , Cmd.batch
        [ Task.attempt (mapMsg << StoreCopyOfBrowserBoard) (Browser.Dom.getElement "main-board")
        ]
    )


makeGameForAnimating : List Move -> Fen -> BasicMove -> Result String ( Logic.Game, BasicMoveWithPieces )
makeGameForAnimating availableMoves (Fen currentState) (BasicMove { from, to }) =
    let
        g =
            Logic.fromFen availableMoves currentState
    in
    case g of
        Ok gg ->
            case Logic.popPiece from gg of
                ( Just ((Logic.Piece team Logic.Monarch) as monarch), ggg ) ->
                    case Logic.castlingMove (Position.toRaw from) (Position.toRaw to) of
                        Just { rookStarting, rookEnding } ->
                            case Logic.popPiece rookStarting ggg of
                                ( Just _, gggg ) ->
                                    Ok ( Logic.nextTurn gggg, CastlingMove { monarchFrom = from, monarchTo = to, team = team, rookFrom = rookStarting, rookTo = rookEnding } )

                                _ ->
                                    Err "No rook found for castlinq!"

                        Nothing ->
                            case Logic.popPiece to ggg of
                                ( pieceTo, gggg ) ->
                                    Ok ( Logic.nextTurn gggg, BasicMoveWithPieces { from = from, to = to, pieceMoved = monarch, pieceCaptured = pieceTo } )

                ( Just p, ggg ) ->
                    case Logic.popPiece to ggg of
                        ( pieceTo, gggg ) ->
                            Ok ( Logic.nextTurn gggg, BasicMoveWithPieces { from = from, to = to, pieceMoved = p, pieceCaptured = pieceTo } )

                ( Nothing, _ ) ->
                    Err "no piece found in move"

        Err fenParsingFailure ->
            Err fenParsingFailure


makeGame : List Move -> Fen -> Logic.Game
makeGame availableMoves (Fen currentState) =
    Result.withDefault Logic.blankBoard <| Logic.fromFen availableMoves currentState


moveMade : List Move -> PreviousMovesSafe -> (Msg -> msg) -> Model -> ( Model, Cmd msg )
moveMade availableMoves (PreviousMovesSafe pm) mapMsg model =
    moveMadeWithAnimation availableMoves (Historical <| List.reverse pm) mapMsg model


moveMadeWithAnimation : List Move -> Historical -> (Msg -> msg) -> Model -> ( Model, Cmd msg )
moveMadeWithAnimation availableMoves (Historical pm) toMsg model =
    let
        ( game, basicMoveWithPieces ) =
            case pm of
                [] ->
                    -- Opening Move shouldn't be possible from this moveMade function.
                    ( DoneAnimating <| makeGame availableMoves model.openingState, Nothing )

                ( basicMove, fenAfterMove ) :: [] ->
                    case makeGameForAnimating availableMoves model.openingState basicMove of
                        Ok ( g, bMWP ) ->
                            if model.madeMoveRecently then
                                ( DoneAnimating <| makeGame availableMoves fenAfterMove
                                , Nothing
                                )

                            else
                                ( Animating { before = g, after = makeGame availableMoves fenAfterMove }
                                , Just bMWP
                                )

                        Err _ ->
                            ( DoneAnimating Logic.blankBoard, Nothing )

                ( basicMove, fenAfterMove ) :: ( _, currentFen ) :: _ ->
                    case makeGameForAnimating availableMoves currentFen basicMove of
                        Ok ( g, bMWP ) ->
                            if model.madeMoveRecently then
                                ( DoneAnimating <| makeGame availableMoves fenAfterMove
                                , Nothing
                                )

                            else
                                ( Animating { before = g, after = makeGame availableMoves fenAfterMove }
                                , Just bMWP
                                )

                        Err e ->
                            ( DoneAnimating Logic.blankBoard, Nothing )
    in
    ( { model | game = game, previousMove = basicMoveWithPieces, madeMoveRecently = False, movesHistorical = Historical pm }
    , Cmd.none
    )



-- Msg


type Msg
    = ChangeTeam Team
    | FindReinforcements Position ()
    | StoreCopyOfBrowserBoard (Result Browser.Dom.Error Browser.Dom.Element)
    | StoreCopyOfNavbar (Result Browser.Dom.Error Browser.Dom.Element)
    | WindowResized
      -- Drag And Drop
    | StartDragging Int Int Position Logic.Piece
    | MoveDragging Int Int
    | StopDragging Int Int
    | MakeMoveIfDragIsOnBoard DragStuffInner (Result Browser.Dom.Error Browser.Dom.Element)



-- UPDATE


update : Callbacks msg -> Msg -> Model -> ( Model, Cmd msg )
update callbacks msg model =
    case msg of
        ChangeTeam color ->
            moveMadeWithAnimation (Dict.toList (mapAnimating model.game |> .moves) |> List.map Tuple.second)
                model.movesHistorical
                callbacks.mapMsg
                { model
                    | playerTeam = color
                    , madeMoveRecently = False
                }

        FindReinforcements currentPosition _ ->
            case model.dragStuff of
                NoPieceInHand ->
                    ( { model | reinforcing = [], opponentReinforcing = [] }, Cmd.none )

                PieceInHand { startingPosition } ->
                    let
                        gg =
                            mapAnimating model.game
                    in
                    ( { model
                        | reinforcing =
                            reinforcingSquares
                                { starting = startingPosition, current = currentPosition }
                                (Logic.canAttack currentPosition gg)
                                gg.moves
                        , opponentReinforcing =
                            reinforcingSquares
                                { starting = startingPosition, current = currentPosition }
                                (Logic.canAttack currentPosition (Logic.nextTurn gg |> Logic.removePiece currentPosition))
                                gg.moves
                      }
                    , Cmd.none
                    )

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
                    ( { model | dragStuff = initialDragStuff, game = DoneAnimating <| removePiece startingPosition (mapAnimating model.game), opponentReinforcing = [] }
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
                                    case findCurrentPosition dragState browserBoard.element (mapAnimating model.game |> .turn) of
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
                    case positionOnBoard dragStuffInner boardElement.element (mapAnimating model.game |> .turn) of
                        Nothing ->
                            ( { model | game = DoneAnimating <| placePiece startingPosition piece (mapAnimating model.game) }
                            , Cmd.none
                            )

                        Just finalPosition ->
                            case friendlyMovesToPosition (mapAnimating model.game |> .turn) finalPosition startingPosition (Dict.toList (mapAnimating model.game |> .moves) |> List.map Tuple.second) of
                                move :: [] ->
                                    ( { model | game = DoneAnimating <| placePiece finalPosition piece (mapAnimating model.game), madeMoveRecently = True }
                                    , Task.perform callbacks.makeMove (Task.succeed move)
                                    )

                                promotionMove :: _ ->
                                    -- TODO: Hardcoded to promote to Advisor.
                                    ( { model | game = DoneAnimating <| placePiece finalPosition piece (mapAnimating model.game), madeMoveRecently = True }
                                    , Task.perform callbacks.makeMove (Task.succeed promotionMove)
                                    )

                                [] ->
                                    ( { model | game = DoneAnimating <| placePiece startingPosition piece (mapAnimating model.game) }
                                    , Cmd.none
                                    )

        StoreCopyOfBrowserBoard result ->
            case result of
                Err err ->
                    ( model, Cmd.none )

                Ok boardElement ->
                    ( { model | browserBoard = Just boardElement }, Cmd.none )

        StoreCopyOfNavbar result ->
            case result of
                Err err ->
                    ( model, Cmd.none )

                Ok navElement ->
                    ( { model | mainNav = Just navElement }, Cmd.none )

        WindowResized ->
            ( model
            , Cmd.batch
                [ Task.attempt (callbacks.mapMsg << StoreCopyOfBrowserBoard) (Browser.Dom.getElement "main-board")
                , Task.attempt (callbacks.mapMsg << StoreCopyOfNavbar) (Browser.Dom.getElement "main-nav")
                ]
            )


mapAnimating : AnimatedGame -> Logic.Game
mapAnimating animatedGame =
    case animatedGame of
        Animating { after } ->
            after

        DoneAnimating g ->
            g



-- GAME LOGIC


{-| Returns all squares that have pieces on them that are able to reinforce the move that is being considered.

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
view { game, movesHistorical, reinforcing, opponentReinforcing, playerTeam, dragStuff, browserBoard, mainNav, previousMove } =
    let
        sideBySide board =
            div [ class "flex flex-col sm:flex-col md:flex-col lg:flex-row xl:flex-row 2xl:flex-row" ]
                [ div [ class "flex-grow flex-shrink-0" ] [ board ]
                , div [ class "flex-shrink-0" ]
                    [ div [] [ h3 [ Attr.class "text-xl h-16" ] [ text "Details" ] ]
                    , viewLegend
                        (Settings
                            { recentMove = True
                            , reinforcing = List.isEmpty reinforcing |> not
                            , opponentReinforcing = List.isEmpty opponentReinforcing |> not
                            }
                        )
                    ]
                ]
    in
    case game of
        Animating { before, after } ->
            sideBySide <|
                div [ class "container mx-auto flex-grow" ]
                    [ lazy2 viewTurn before playerTeam
                    , Maybe.map2 (lazy4 viewPreviousMove before playerTeam) previousMove browserBoard |> Maybe.withDefault (span [] [])
                    , lazy3 viewBoard (SquareStuff before movesHistorical reinforcing opponentReinforcing playerTeam) dragStuff browserBoard
                    , lazy viewSettings playerTeam
                    ]

        DoneAnimating g ->
            sideBySide <|
                div [ class "container mx-auto flex-grow" ]
                    [ lazy2 viewTurn g playerTeam
                    , Maybe.map (lazy4 viewGhostSquare g playerTeam dragStuff) browserBoard |> Maybe.withDefault (span [] [])
                    , lazy3 viewBoard (SquareStuff g movesHistorical reinforcing opponentReinforcing playerTeam) dragStuff browserBoard
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
            text "You are up."

          else
            --text "Opponents turn"
            text "Waiting on them!"
        ]


type alias SquareStuff =
    { game : Logic.Game
    , historical : Historical
    , reinforcing : List Position
    , opponentReinforcing : List Position
    , playerColor : Team
    }


viewBoard : SquareStuff -> DragStuff -> Maybe Browser.Dom.Element -> Html Msg
viewBoard ({ game, playerColor, historical } as cellStuff) dragStuff browserBoard =
    div []
        [ div
            [ id "main-board"
            , classList
                [ ( "h-96 w-96 md:h-constrained-1/2 md:w-constrained-1/2 lg:h-constrained-40% lg:w-constrained-40% 2xl:h-constrained-40% 2xl:w-constrained-40% grid grid-cols-8 grid-rows-8 border-2 border-gray-500 gap-0 shadow-2xl", True )
                , ( "rotated", playerColor == Logic.White )
                ]
            ]
            (List.concatMap (viewRow cellStuff browserBoard) (List.range 1 8))
        ]


viewRow : SquareStuff -> Maybe Browser.Dom.Element -> Int -> List (Html Msg)
viewRow cellStuff browserBoard row =
    List.map (lazy4 viewCell cellStuff browserBoard row) (List.reverse (List.range 1 8))


viewCell : SquareStuff -> Maybe Browser.Dom.Element -> Int -> Int -> Html Msg
viewCell ({ playerColor } as cellStuff) browserBoard row column =
    div
        [ classList
            [ ( "column-" ++ String.fromInt column, True )
            , ( "row-" ++ String.fromInt row, True )
            , ( shading column row, True )
            , ( "rotated", playerColor == Logic.White )
            ]
        ]
        [ lazy3 viewSquare cellStuff browserBoard (Position column row) ]


viewGhostSquare : Logic.Game -> Team -> DragStuff -> Browser.Dom.Element -> Html Msg
viewGhostSquare game playerColor dragStuff browserBoard =
    let
        squareSize =
            round <| browserBoard.element.width / 8
    in
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
                    , ( "fixed", True )
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


viewPreviousMove : Logic.Game -> Team -> BasicMoveWithPieces -> Browser.Dom.Element -> Html Msg
viewPreviousMove game playerColor previousMove browserBoard =
    let
        squareSize =
            browserBoard.element.width / 8
    in
    case previousMove of
        BasicMoveWithPieces { from, to, pieceMoved, pieceCaptured } ->
            div []
                [ Animated.div (runFull { starting = Position.toRaw from, diff = Position.toRaw to } playerColor squareSize)
                    [ classList
                        [ ( "grid-cols-none", True )
                        , ( "absolute", True )
                        , ( "z-50", True )

                        --, ( "border-2", True )
                        ]
                    , styleList
                        [ "width: " ++ String.fromInt (round squareSize) ++ "px"
                        , "height: " ++ String.fromInt (round squareSize) ++ "px"
                        ]
                    ]
                    [ findSvg pieceMoved [] ]
                , case pieceCaptured of
                    Nothing ->
                        div [] []

                    Just pC ->
                        Animated.div (runHide { starting = Position.toRaw to } playerColor squareSize)
                            [ classList
                                [ ( "grid-cols-none", True )
                                , ( "absolute", True )
                                , ( "z-50", True )
                                ]
                            , styleList
                                [ "width: " ++ String.fromInt (round squareSize) ++ "px"
                                , "height: " ++ String.fromInt (round squareSize) ++ "px"
                                ]
                            ]
                            [ findSvg pC [] ]
                ]

        CastlingMove { monarchFrom, monarchTo, rookFrom, rookTo, team } ->
            div []
                [ Animated.div (runFull { starting = Position.toRaw monarchFrom, diff = Position.toRaw monarchTo } playerColor squareSize)
                    [ classList
                        [ ( "grid-cols-none", True )
                        , ( "absolute", True )
                        , ( "z-50", True )
                        ]
                    , styleList
                        [ "width: " ++ String.fromInt (round squareSize) ++ "px"
                        , "height: " ++ String.fromInt (round squareSize) ++ "px"
                        ]
                    ]
                    [ findSvg (Logic.Piece team Logic.Monarch) [] ]
                , Animated.div (runFull { starting = Position.toRaw rookFrom, diff = Position.toRaw rookTo } playerColor squareSize)
                    [ classList
                        [ ( "grid-cols-none", True )
                        , ( "absolute", True )
                        , ( "z-50", True )
                        ]
                    , styleList
                        [ "width: " ++ String.fromInt (round squareSize) ++ "px"
                        , "height: " ++ String.fromInt (round squareSize) ++ "px"
                        ]
                    ]
                    [ findSvg (Logic.Piece team Logic.Rook) [] ]
                ]


runHide : { starting : ( Int, Int ) } -> Team -> Float -> Animation
runHide { starting } playerColor squareSize =
    let
        borderSize =
            -2
    in
    Animation.steps { startAt = [ squareLocation starting squareSize borderSize playerColor |> (\( x, y ) -> P.xy x y) ], options = [ Animation.delay 500 ] }
        [ Animation.step 400 [ P.opacity 0, squareLocation starting squareSize borderSize playerColor |> (\( x, y ) -> P.xy x y) ]
        ]


runFull : { starting : ( Int, Int ), diff : ( Int, Int ) } -> Team -> Float -> Animation
runFull { starting, diff } playerColor squareSize =
    let
        borderSize =
            case playerColor of
                Logic.Black ->
                    0

                Logic.White ->
                    0
    in
    Animation.steps
        { startAt =
            [ squareLocation starting squareSize borderSize playerColor |> (\( x, y ) -> P.xy x y)
            ]
        , options = [ Animation.easeInOut ]
        }
        [ Animation.wait 500
        , Animation.step 400 [ squareLocation diff squareSize borderSize playerColor |> (\( x, y ) -> P.xy x y) ]
        , Animation.wait 300
        , Animation.step 500
            [ squareLocation diff squareSize borderSize playerColor |> (\( x, y ) -> P.xy x y)
            , P.backgroundColor "rgba(156, 163, 175, 0.7)"
            ]
        ]


squareLocation : ( Int, Int ) -> Float -> Float -> Team -> ( Float, Float )
squareLocation ( squareDiffX, squareDiffY ) squareSize borderSize playerColor =
    let
        xPerspectiveCorrected =
            case playerColor of
                Logic.Black ->
                    toFloat (8 - squareDiffX)

                Logic.White ->
                    toFloat (squareDiffX - 1)

        yPerspectiveCorrected =
            case playerColor of
                Logic.Black ->
                    toFloat (squareDiffY - 1)

                Logic.White ->
                    toFloat (8 - squareDiffY)
    in
    ( xPerspectiveCorrected * squareSize + borderSize, yPerspectiveCorrected * squareSize + borderSize )


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


viewSquare : SquareStuff -> Maybe Browser.Dom.Element -> Position -> Html Msg
viewSquare { game, historical, reinforcing, opponentReinforcing, playerColor } browserBoard position =
    case ( game.turn == playerColor, Dict.get (Position.toRaw position) game.occupiedSquares ) of
        ( True, Just piece ) ->
            let
                additionalSquareClasses =
                    [ ( "bg-green-500", reinforces position reinforcing )
                    , ( "bg-yellow-500", reinforces position opponentReinforcing )
                    , ( Position.toAlgebraic position, True )
                    ]

                additionalSquareAttrs =
                    [ style "cursor: grab"
                    , on "mousedown" (D.map4 StartDragging pageX pageY (D.succeed position) (D.succeed piece))
                    ]
            in
            basicSquare position historical additionalSquareClasses additionalSquareAttrs [ findSvg piece [] ]

        ( False, Just piece ) ->
            basicSquare position historical [] [] [ findSvg piece [] ]

        ( _, Nothing ) ->
            basicSquare position historical [] [] []


basicSquare position historical additionalSquareClasses additionalSquareAttrs additionalSquareChildren =
    div
        ([ classList
            ([ ( "w-full h-full m-0", True )
             , ( "bg-gray-400 bg-opacity-70", isRecentMove position historical )
             , ( "transition", True )
             ]
                ++ additionalSquareClasses
            )
         ]
            ++ additionalSquareAttrs
        )
        additionalSquareChildren


isRecentMove : Position -> Historical -> Bool
isRecentMove position (Historical directional) =
    case directional of
        [] ->
            False

        ( BasicMove { from, to }, _ ) :: _ ->
            from == position || to == position


reinforces : Position -> List Position -> Bool
reinforces position reinforcing =
    List.any ((==) position) reinforcing


shading : Int -> Int -> String
shading column row =
    if modBy 2 (column + row) == 0 then
        "bg-gray-200 bg-opacity-50"

    else
        "bg-indigo-400 bg-opacity-50"



-- SQUARE PROPERTIES


friendlyMovesToPosition : Team -> Position -> Position -> List Move -> List Move
friendlyMovesToPosition turn squareTo squareFrom moves =
    movesToPosition squareTo squareFrom moves
        |> List.filter (\move -> move.color == turnToColor turn)



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
    Sub.batch
        [ case model.dragStuff of
            NoPieceInHand ->
                Sub.none

            PieceInHand _ ->
                onMouseMove (D.map2 MoveDragging pageX pageY)
        , Browser.Events.onResize (\w h -> WindowResized)
        ]


pageX : D.Decoder Int
pageX =
    D.field "pageX" D.int


pageY : D.Decoder Int
pageY =
    D.field "pageY" D.int



--- Generated elm
{- This example requires Tailwind CSS v2.0+ -}


type Settings
    = Settings { recentMove : Bool, reinforcing : Bool, opponentReinforcing : Bool }


viewLegend (Settings { recentMove, reinforcing, opponentReinforcing }) =
    div
        [ Attr.class "flow-root "
        ]
        [ div [ Attr.class "" ]
            [ ul
                [ Attr.class "-mb-8"
                ]
                [ li []
                    [ div
                        [ Attr.class "relative pb-8"
                        ]
                        [ span
                            [ Attr.class "absolute top-4 left-4 -ml-px h-full w-0.5 bg-gray-200"
                            , Attr.attribute "aria-hidden" "true"
                            ]
                            []
                        , div
                            [ Attr.class "relative flex space-x-3"
                            ]
                            [ div []
                                [ span
                                    [ Attr.class "h-8 w-8 rounded-full bg-gray-400 flex items-center justify-center ring-8 ring-white"
                                    ]
                                    [ {- Heroicon name: solid/user -}
                                      svg
                                        [ SvgAttr.class "h-5 w-5 text-white"
                                        , SvgAttr.viewBox "0 0 20 20"
                                        , SvgAttr.fill "currentColor"
                                        , Attr.attribute "aria-hidden" "true"
                                        ]
                                        [ path
                                            [ SvgAttr.fillRule "evenodd"
                                            , SvgAttr.d "M10 9a3 3 0 100-6 3 3 0 000 6zm-7 9a7 7 0 1114 0H3z"
                                            , SvgAttr.clipRule "evenodd"
                                            ]
                                            []
                                        ]
                                    ]
                                ]
                            , div
                                [ Attr.class "min-w-0 flex-1 pt-1.5 flex justify-between space-x-4"
                                ]
                                [ div []
                                    [ p
                                        [ Attr.class "text-sm text-gray-500"
                                        ]
                                        [ text "Recent Move"
                                        ]
                                    ]

                                --, div
                                --    [ Attr.class "text-right text-sm whitespace-nowrap text-gray-500"
                                --    ]
                                --    [ text ":wave: Hover here.  " ]
                                --[ time
                                --    [ Attr.datetime "2020-09-20"
                                --    ]
                                --    [ text "Sep 20" ]
                                --]
                                ]
                            ]
                        ]
                    ]

                --, li []
                --    [ div
                --        [ Attr.class "relative pb-8"
                --        ]
                --        [ span
                --            [ Attr.class "absolute top-4 left-4 -ml-px h-full w-0.5 bg-gray-200"
                --            , Attr.attribute "aria-hidden" "true"
                --            ]
                --            []
                --        , div
                --            [ Attr.class "relative flex space-x-3"
                --            ]
                --            [ div []
                --                [ span
                --                    [ Attr.class "h-8 w-8 rounded-full bg-blue-500 flex items-center justify-center ring-8 ring-white"
                --                    ]
                --                    [ {- Heroicon name: solid/thumb-up -}
                --                      svg
                --                        [ SvgAttr.class "h-5 w-5 text-white"
                --                        , SvgAttr.viewBox "0 0 20 20"
                --                        , SvgAttr.fill "currentColor"
                --                        , Attr.attribute "aria-hidden" "true"
                --                        ]
                --                        [ path
                --                            [ SvgAttr.d "M2 10.5a1.5 1.5 0 113 0v6a1.5 1.5 0 01-3 0v-6zM6 10.333v5.43a2 2 0 001.106 1.79l.05.025A4 4 0 008.943 18h5.416a2 2 0 001.962-1.608l1.2-6A2 2 0 0015.56 8H12V4a2 2 0 00-2-2 1 1 0 00-1 1v.667a4 4 0 01-.8 2.4L6.8 7.933a4 4 0 00-.8 2.4z"
                --                            ]
                --                            []
                --                        ]
                --                    ]
                --                ]
                --            , div
                --                [ Attr.class "min-w-0 flex-1 pt-1.5 flex justify-between space-x-4"
                --                ]
                --                [ div []
                --                    [ p
                --                        [ Attr.class "text-sm text-gray-500"
                --                        ]
                --                        [ text "Advanced to phone screening by"
                --                        , a
                --                            [ Attr.href "#"
                --                            , Attr.class "font-medium text-gray-900"
                --                            ]
                --                            [ text "Bethany Blake" ]
                --                        ]
                --                    ]
                --                , div
                --                    [ Attr.class "text-right text-sm whitespace-nowrap text-gray-500"
                --                    ]
                --                    [ time
                --                        [ Attr.datetime "2020-09-22"
                --                        ]
                --                        [ text "Sep 22" ]
                --                    ]
                --                ]
                --            ]
                --        ]
                --    ]
                , viewLegendOption reinforcing "bg-green-500" "Reinforcing Move"
                , viewLegendOption opponentReinforcing "bg-yellow-500" "Opponent Threatens"

                --, li []
                --    [ div
                --        [ Attr.class "relative pb-8"
                --        ]
                --        [ span
                --            [ Attr.class "absolute top-4 left-4 -ml-px h-full w-0.5 bg-gray-200"
                --            , Attr.attribute "aria-hidden" "true"
                --            ]
                --            []
                --        , div
                --            [ Attr.class "relative flex space-x-3"
                --            ]
                --            [ div []
                --                [ span
                --                    [ Attr.class "h-8 w-8 rounded-full bg-blue-500 flex items-center justify-center ring-8 ring-white"
                --                    ]
                --                    [ {- Heroicon name: solid/thumb-up -}
                --                      svg
                --                        [ SvgAttr.class "h-5 w-5 text-white"
                --                        , SvgAttr.viewBox "0 0 20 20"
                --                        , SvgAttr.fill "currentColor"
                --                        , Attr.attribute "aria-hidden" "true"
                --                        ]
                --                        [ path
                --                            [ SvgAttr.d "M2 10.5a1.5 1.5 0 113 0v6a1.5 1.5 0 01-3 0v-6zM6 10.333v5.43a2 2 0 001.106 1.79l.05.025A4 4 0 008.943 18h5.416a2 2 0 001.962-1.608l1.2-6A2 2 0 0015.56 8H12V4a2 2 0 00-2-2 1 1 0 00-1 1v.667a4 4 0 01-.8 2.4L6.8 7.933a4 4 0 00-.8 2.4z"
                --                            ]
                --                            []
                --                        ]
                --                    ]
                --                ]
                --            , div
                --                [ Attr.class "min-w-0 flex-1 pt-1.5 flex justify-between space-x-4"
                --                ]
                --                [ div []
                --                    [ p
                --                        [ Attr.class "text-sm text-gray-500"
                --                        ]
                --                        [ text "Advanced to interview by"
                --                        , a
                --                            [ Attr.href "#"
                --                            , Attr.class "font-medium text-gray-900"
                --                            ]
                --                            [ text "Bethany Blake" ]
                --                        ]
                --                    ]
                --                , div
                --                    [ Attr.class "text-right text-sm whitespace-nowrap text-gray-500"
                --                    ]
                --                    [ time
                --                        [ Attr.datetime "2020-09-30"
                --                        ]
                --                        [ text "Sep 30" ]
                --                    ]
                --                ]
                --            ]
                --        ]
                --    ]
                --, li []
                --    [ div
                --        [ Attr.class "relative pb-8"
                --        ]
                --        [ div
                --            [ Attr.class "relative flex space-x-3"
                --            ]
                --            [ div []
                --                [ span
                --                    [ Attr.class "h-8 w-8 rounded-full bg-green-500 flex items-center justify-center ring-8 ring-white"
                --                    ]
                --                    [ {- Heroicon name: solid/check -}
                --                      svg
                --                        [ SvgAttr.class "h-5 w-5 text-white"
                --                        , SvgAttr.viewBox "0 0 20 20"
                --                        , SvgAttr.fill "currentColor"
                --                        , Attr.attribute "aria-hidden" "true"
                --                        ]
                --                        [ path
                --                            [ SvgAttr.fillRule "evenodd"
                --                            , SvgAttr.d "M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z"
                --                            , SvgAttr.clipRule "evenodd"
                --                            ]
                --                            []
                --                        ]
                --                    ]
                --                ]
                --            , div
                --                [ Attr.class "min-w-0 flex-1 pt-1.5 flex justify-between space-x-4"
                --                ]
                --                [ div []
                --                    [ p
                --                        [ Attr.class "text-sm text-gray-500"
                --                        ]
                --                        [ text "Completed interview with"
                --                        , a
                --                            [ Attr.href "#"
                --                            , Attr.class "font-medium text-gray-900"
                --                            ]
                --                            [ text "Katherine Snyder" ]
                --                        ]
                --                    ]
                --                , div
                --                    [ Attr.class "text-right text-sm whitespace-nowrap text-gray-500"
                --                    ]
                --                    [ time
                --                        [ Attr.datetime "2020-10-04"
                --                        ]
                --                        [ text "Oct 4" ]
                --                    ]
                --                ]
                --            ]
                --        ]
                --    ]
                ]
            ]
        ]


viewLegendOption display displayColor displayText =
    if display then
        li []
            [ div
                [ Attr.class "relative pb-8"
                ]
                [ span
                    [ Attr.class "absolute top-4 left-4 -ml-px h-full w-0.5 bg-gray-200"
                    , Attr.attribute "aria-hidden" "true"
                    ]
                    []
                , div
                    [ Attr.class "relative flex space-x-3"
                    ]
                    [ div []
                        [ --span
                          --    [ Attr.class "h-8 w-8 rounded-full bg-gray-400 flex items-center justify-center ring-8 ring-white"
                          --    ]
                          --    [ {- Heroicon name: solid/user -}
                          --      svg
                          --        [ SvgAttr.class "h-5 w-5 text-white"
                          --        , SvgAttr.viewBox "0 0 20 20"
                          --        , SvgAttr.fill "currentColor"
                          --        , Attr.attribute "aria-hidden" "true"
                          --        ]
                          --        [ path
                          --            [ SvgAttr.fillRule "evenodd"
                          --            , SvgAttr.d "M10 9a3 3 0 100-6 3 3 0 000 6zm-7 9a7 7 0 1114 0H3z"
                          --            , SvgAttr.clipRule "evenodd"
                          --            ]
                          --            []
                          --        ]
                          --    ]
                          span
                            [ Attr.class <| "h-8 w-8 rounded-full " ++ displayColor ++ " flex items-center justify-center ring-8 ring-white"
                            ]
                            [ {- Heroicon name: solid/check -}
                              svg
                                [ SvgAttr.class "h-5 w-5 text-white"
                                , SvgAttr.viewBox "0 0 20 20"
                                , SvgAttr.fill "currentColor"
                                , Attr.attribute "aria-hidden" "true"
                                ]
                                [ path
                                    [ SvgAttr.fillRule "evenodd"
                                    , SvgAttr.d "M10 9a3 3 0 100-6 3 3 0 000 6zm-7 9a7 7 0 1114 0H3z"
                                    , SvgAttr.clipRule "evenodd"
                                    ]
                                    []

                                --path
                                --    [ SvgAttr.fillRule "evenodd"
                                --    , SvgAttr.d "M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z"
                                --    , SvgAttr.clipRule "evenodd"
                                --    ]
                                --    []
                                ]
                            ]
                        ]
                    , div
                        [ Attr.class "min-w-0 flex-1 pt-1.5 flex justify-between space-x-4"
                        ]
                        [ div []
                            [ p
                                [ Attr.class "text-sm text-gray-500"
                                ]
                                [ text displayText
                                ]
                            ]

                        --, div
                        --    [ Attr.class "text-right text-sm whitespace-nowrap text-gray-500"
                        --    ]
                        --    [ text "Learn more." ]
                        ]
                    ]
                ]
            ]

    else
        span [] []
