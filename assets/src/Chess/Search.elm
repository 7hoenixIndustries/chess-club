module Chess.Search exposing
    ( Accumulator(..)
    , BestFound(..)
    , DeepestNodeReached(..)
    , Failure(..)
    , ForcingWeight(..)
    , Model
    , Msg(..)
    , Stats(..)
    , forcingMoves
    , forcingMovesWithStats
    , init
    , update
    , view
    )

import Chess.Board as Position exposing (Position(..))
import Chess.Game as Game exposing (Game(..), Square, Turn)
import Dict exposing (Dict)
import Graph.Tree as Tree exposing (Tree)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias Model =
    { game : Game
    , forcedMoves : Maybe (List String)

    -- V2
    , moves : List SearchMove
    }


type alias SearchMove =
    String


type Msg
    = FindForcingMoves
    | FindForcingMovesWithStats


init : Game -> Model
init game =
    Model game Nothing []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FindForcingMoves ->
            ( { model | forcedMoves = Just <| forcingMoves model }, Cmd.none )

        FindForcingMovesWithStats ->
            ( model, Cmd.none )


positionToSquareKey : Position -> ( Int, Int )
positionToSquareKey (Position column row) =
    ( column, row )



-- FORCING MOVES WITH STATS
{-
   Stats may be returned if desired from ForcingMoves.

   forcingMovesWithStats : Model -> Result Search.Failure (Search.ForcingMovesPayload, Search.Stats)
-}


forcingMovesWithStats : Model -> Result Failure ( BestFound, Stats )
forcingMovesWithStats { game } =
    let
        childrenThatResolveToCheckmate =
            findCheckmatesWithStats game
    in
    case childrenThatResolveToCheckmate of
        finishingMove :: otherMovesThatAlsoWin ->
            Ok ( BestFound finishingMove otherMovesThatAlsoWin, Stats <| DeepestNodeReached 0 )

        _ ->
            --Game.canMoveTo
            --if Game.isCheckmate game then
            --    Ok <| BestFound
            Err Failure


findCheckmatesWithStats : Game -> List SearchMove
findCheckmatesWithStats game =
    List.concatMap (findCheckmatesForPositionWithStats game) Position.all


findCheckmatesForPositionWithStats : Game -> Position -> List SearchMove
findCheckmatesForPositionWithStats game ((Position column row) as squareTo) =
    Game.canMoveTo ( column, row ) game
        |> List.map (\( c, r ) -> Position c r)
        |> List.filter (\squareFrom -> leadsToCheckmate squareFrom squareTo game)
        |> List.map (\squareFromThatIsCheckmate -> squaresToMoveKey squareFromThatIsCheckmate squareTo)


squaresToMoveKey : Position -> Position -> String
squaresToMoveKey squareFrom squareTo =
    Position.toString squareFrom ++ "-" ++ Position.toString squareTo


type Stats
    = Stats DeepestNodeReached


type DeepestNodeReached
    = DeepestNodeReached Int


type Failure
    = Failure



-- Consider promoting this to Prelude
--type ActiveList current
--    = ActiveList (Viewed current) current (Remaining current)
--
--type Viewed current = Viewed current
--type Remaining current = Remaining current


type BestFound
    = BestFound String (List String)



-- FORCING MOVES


type ForcingWeight
    = CheckMate
    | Check
    | Forced


type alias ForcingMove =
    { game : Game
    , squareTo : Position
    , squareFrom : Position
    , value : ForcingWeight
    , path : Path
    }


forcingMoves : Model -> List String
forcingMoves { game } =
    Tree.unfoldForest next (findForcingMoves game (Path []))
        |> Tree.inner (Root game)
        |> Tree.levelOrder visit (Accumulator [] 100)
        |> findForcingMovesPath


findForcingMovesPath : Accumulator -> List String
findForcingMovesPath (Accumulator nodes _) =
    List.map findForcingMovePath nodes


findForcingMovePath : Node -> String
findForcingMovePath node =
    case node of
        Root _ ->
            "Root"

        NotFound ->
            "Not Found"

        Node game forcingMove ->
            String.join " > "
                [ fMoveToString forcingMove
                , pathToString forcingMove.path
                ]


fMoveToString : ForcingMove -> String
fMoveToString forcingMove =
    Position.toString forcingMove.squareFrom ++ "-" ++ Position.toString forcingMove.squareTo ++ " " ++ weightToString forcingMove.value


weightToString : ForcingWeight -> String
weightToString weight =
    case weight of
        CheckMate ->
            "CheckMate"

        Check ->
            "Check"

        Forced ->
            "Forced"


pathToString : Path -> String
pathToString (Path path) =
    if List.isEmpty path then
        "Root"

    else
        String.join " > "
            (List.map fMoveToString path)



-- TREE TRAVERSAL


type Accumulator
    = Accumulator (List Node) Int


visit : Node -> Tree.Forest Node -> Accumulator -> Accumulator
visit node children ((Accumulator nodes best) as acc) =
    case node of
        Node game forcingMove ->
            case forcingMove.value of
                CheckMate ->
                    Accumulator (node :: nodes) best

                _ ->
                    acc

        _ ->
            acc



-- TREE POPULATION


type Node
    = Node Game ForcingMove
    | NotFound
    | Root Game


type Path
    = Path (List ForcingMove)


next : ForcingMove -> ( Node, List ForcingMove )
next ({ value, squareFrom, squareTo, game, path } as forcingMove) =
    if depth path > 4 then
        ( NotFound, [] )

    else
        case value of
            Check ->
                let
                    nextGame =
                        Game.makeMove (positionToSquareKey squareFrom) (positionToSquareKey squareTo) game

                    forced =
                        filterOutComplexMoves <| findMovesThatAvoidCheck nextGame (appendPath forcingMove path)
                in
                ( Node nextGame forcingMove, forced )

            Forced ->
                let
                    nextGame =
                        Game.makeMove (positionToSquareKey squareFrom) (positionToSquareKey squareTo) game

                    forcing =
                        findForcingMoves nextGame (appendPath forcingMove path)
                in
                ( Node nextGame forcingMove, forcing )

            CheckMate ->
                ( Node (Game.makeMove (positionToSquareKey squareFrom) (positionToSquareKey squareTo) game) forcingMove, [] )



-- NOTE: This is used to restrict the computation so that we are only analyzing positions that don't fan out.
-- It is the same as when analyzing a game as a human (computations are only really feasible up to a few different options).
-- And then go systematically through the options.


filterOutComplexMoves : List ForcingMove -> List ForcingMove
filterOutComplexMoves moves =
    if List.length moves > 3 then
        []

    else
        moves


depth : Path -> Int
depth (Path path) =
    List.length path


appendPath : ForcingMove -> Path -> Path
appendPath forcingMove (Path path) =
    Path <| forcingMove :: path


findForcingMoves : Game -> Path -> List ForcingMove
findForcingMoves game path =
    let
        checkmates =
            findCheckmates game path

        checks =
            findPotentialChecks game path
    in
    checkmates ++ checks



-- AVOID CHECK


findMovesThatAvoidCheck : Game -> Path -> List ForcingMove
findMovesThatAvoidCheck game path =
    List.concatMap (findMovesThatAvoidCheckForPosition game path) Position.all


findMovesThatAvoidCheckForPosition : Game -> Path -> Position -> List ForcingMove
findMovesThatAvoidCheckForPosition game path ((Position column row) as squareTo) =
    Game.canMoveTo (positionToSquareKey squareTo) game
        |> List.map (\( c, r ) -> Position c r)
        |> List.filter (\squareFrom -> avoidsCheck squareFrom squareTo game)
        |> List.map (\squareFromThatAvoids -> ForcingMove game squareTo squareFromThatAvoids Forced path)


avoidsCheck : Position -> Position -> Game -> Bool
avoidsCheck squareFrom squareTo ((Game occupiedSquares moves ((Game.Turn team) as turn)) as game) =
    let
        occupiedAsList =
            Dict.toList occupiedSquares

        -- TODO: pass in monarchs so that we know we have them.
        ( monarchLocation, monarch ) =
            List.filter (Game.sameTeam turn) occupiedAsList
                |> List.filter Game.isMonarch
                |> List.head
                |> Maybe.withDefault ( ( 1, 1 ), Game.Piece team Game.Monarch )

        monarchVectors =
            Game.possibleMonarchVectors monarchLocation

        monarchIsAbleToEscape =
            List.filter (\monarchMoveTo -> Game.monarchCanMoveTo game monarchMoveTo occupiedSquares monarchLocation team) monarchVectors
                |> List.isEmpty
                |> not
    in
    -- TODO: Add blockable &
    monarchIsAbleToEscape


findCheckmates : Game -> Path -> List ForcingMove
findCheckmates game path =
    List.concatMap (findCheckmatesForPosition game path) Position.all


findCheckmatesForPosition : Game -> Path -> Position -> List ForcingMove
findCheckmatesForPosition game path ((Position column row) as squareTo) =
    Game.canMoveTo ( column, row ) game
        |> List.map (\( c, r ) -> Position c r)
        |> List.filter (\squareFrom -> leadsToCheckmate squareFrom squareTo game)
        |> List.map (\squareFromThatIsCheckmate -> ForcingMove game squareTo squareFromThatIsCheckmate CheckMate path)


leadsToCheckmate : Position -> Position -> Game -> Bool
leadsToCheckmate squareFrom squareTo game =
    Game.makeMove (positionToSquareKey squareFrom) (positionToSquareKey squareTo) game
        |> Game.isCheckmate



-- CHECKS


findPotentialChecks : Game -> Path -> List ForcingMove
findPotentialChecks game path =
    List.concatMap (findChecksForPosition game path) Position.all


findChecksForPosition : Game -> Path -> Position -> List ForcingMove
findChecksForPosition game path ((Position column row) as squareTo) =
    Game.canMoveTo ( column, row ) game
        |> List.map (\( c, r ) -> Position c r)
        |> List.filter (\squareFrom -> leadsToCheck squareFrom squareTo game)
        |> List.map (\squareFromThatIsCheck -> ForcingMove game squareTo squareFromThatIsCheck Check path)


leadsToCheck : Position -> Position -> Game -> Bool
leadsToCheck squareFrom squareTo game =
    Game.makeMove (positionToSquareKey squareFrom) (positionToSquareKey squareTo) game
        |> Game.findChecks
        |> (not << List.isEmpty)



-- VIEW


view : Model -> Html Msg
view { forcedMoves } =
    case forcedMoves of
        Nothing ->
            div [ class "container" ] [ button [ onClick FindForcingMoves, class "bg-red-500 hover:bg-red-700 rounded-md" ] [ text "Find Forcing Moves" ] ]

        Just [] ->
            div [ class "container" ]
                [ text "No forcing moves found for this position."
                , button [ onClick FindForcingMoves, class "bg-red-500 hover:bg-red-700 rounded-md" ] [ text "Find again." ]
                ]

        Just ((move :: _) as moves) ->
            div [ class "container" ]
                [ text <| move ++ " is a forcing move."
                , text <| "Total found: " ++ String.fromInt (List.length moves + 1)
                , button [ onClick FindForcingMoves, class "bg-red-500 hover:bg-red-700 rounded-md" ] [ text "Find again." ]
                ]
