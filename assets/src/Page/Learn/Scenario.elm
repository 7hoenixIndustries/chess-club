module Page.Learn.Scenario exposing
    ( BasicMove(..)
    , Fen(..)
    , Move
    , MoveCommand(..)
    , PreviousMovesSafe(..)
    , Scenario
    , Scenario2(..)
    , ScenarioSeed
    , createScenario
    , getScenario
    , getScenarios
    , makeMove
    , moveSelection
    , scenarioSelection
    , subscribeToMoves
    )

import Api.Mutation as Mutation
import Api.Object
import Api.Object.Move
import Api.Object.MovePlayed
import Api.Object.Scenario exposing (availableMoves, currentState, id, movesPlayed, startingState)
import Api.Query exposing (scenario, scenarios)
import Api.Scalar exposing (Id(..))
import Api.Subscription as Subscription
import Backend exposing (Backend)
import Chess.Position as Position exposing (Position)
import Graphql.Http exposing (Request)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, list, with)
import Json.Decode as D
import Json.Encode as E
import Result.Extra



-- SCENARIO
-- TODO: Lock this down.


type Scenario2
    = Loaded Scenario
    | Seed ScenarioSeed


type alias Scenario =
    { startingState : Fen
    , availableMoves : List Move
    , currentState : Fen
    , previousMoves : PreviousMovesSafe
    , id : Id
    }


{-| Moves that have been played with list of tuples with the command and corresponding fen that comes up afterwards.
-}
type PreviousMovesSafe
    = PreviousMovesSafe (List ( BasicMove, Fen ))


type Fen
    = Fen String


type MoveCommand
    = MoveCommand String


type BasicMove
    = BasicMove { from : Position, to : Position }


scenarioQuery : Id -> SelectionSet Scenario RootQuery
scenarioQuery id =
    (scenario <| { scenarioId = id }) scenarioSelection


scenarioSelection : SelectionSet Scenario Api.Object.Scenario
scenarioSelection =
    SelectionSet.map5 Scenario
        (SelectionSet.map Fen startingState)
        (availableMoves moveSelection)
        (SelectionSet.map Fen currentState)
        movesPlayedSelection
        id


mPS mp =
    let
        moveNonNull : ( MoveCommand, Fen ) -> Result String ( BasicMove, Fen )
        moveNonNull ( MoveCommand mc, fen ) =
            case String.toList mc of
                [ a, b, c, d ] ->
                    (Result.map2
                        (\from to -> BasicMove { from = from, to = to })
                        (D.decodeValue Position.decoder (E.string (String.fromList [ a, b ])))
                        (D.decodeValue Position.decoder (E.string (String.fromList [ c, d ])))
                        |> Result.mapError (\_ -> "Not a valid move")
                    )
                        |> Result.map (\basicMove -> ( basicMove, fen ))

                _ ->
                    Err "Unhandled move variant!"
    in
    Result.Extra.combine (List.map moveNonNull mp)
        |> Result.andThen (\foo -> Ok (PreviousMovesSafe foo))


movesPlayedSelection : SelectionSet PreviousMovesSafe Api.Object.Scenario
movesPlayedSelection =
    SelectionSet.mapOrFail mPS
        (movesPlayed movePlayedSelection)


movePlayedSelection : SelectionSet ( MoveCommand, Fen ) Api.Object.MovePlayed
movePlayedSelection =
    SelectionSet.succeed (\rawPrevious rawFen -> Tuple.pair (MoveCommand rawPrevious) (Fen rawFen))
        |> with Api.Object.MovePlayed.previousMove
        |> with Api.Object.MovePlayed.fenAfterMove


getScenario : Backend -> Id -> (Result (Graphql.Http.Error Scenario) Scenario -> msg) -> Cmd msg
getScenario backend id msg =
    Backend.sendAuthorizedQuery backend (scenarioQuery id) msg



-- SCENARIO SEEDS


type alias ScenarioSeed =
    { id : Id
    }


scenariosQuery : SelectionSet (List ScenarioSeed) RootQuery
scenariosQuery =
    scenarios scenarioSeedSelection


scenarioSeedSelection : SelectionSet ScenarioSeed Api.Object.Scenario
scenarioSeedSelection =
    SelectionSet.map ScenarioSeed
        id


getScenarios : Backend -> (Result (Graphql.Http.Error (List ScenarioSeed)) (List ScenarioSeed) -> msg) -> Cmd msg
getScenarios backend msg =
    Backend.sendAuthorizedQuery backend scenariosQuery msg



-- MOVE


type alias Move =
    { fenAfterMove : String
    , squareFrom : String
    , squareTo : String
    , color : String
    , moveCommand : String
    }


moveSelection : SelectionSet Move Api.Object.Move
moveSelection =
    SelectionSet.succeed Move
        |> with Api.Object.Move.fenAfterMove
        |> with Api.Object.Move.squareFrom
        |> with Api.Object.Move.squareTo
        |> with Api.Object.Move.color
        |> with Api.Object.Move.moveCommand



-- This only sends as we are subscribing to the result


sendMakeMove : Id -> Move -> SelectionSet () RootMutation
sendMakeMove id { moveCommand } =
    Mutation.makeMove { moveCommand = moveCommand, scenarioId = id } SelectionSet.empty
        |> SelectionSet.map (\_ -> ())


makeMove : Backend -> Id -> Move -> (Result (Graphql.Http.Error ()) () -> msg) -> Cmd msg
makeMove backend id move msg =
    Backend.sendAuthorizedMutation backend (sendMakeMove id move) msg



-- SUBSCRIBE TO MOVES


subscribeToMoves : Id -> SelectionSet Scenario RootSubscription
subscribeToMoves id =
    Subscription.moveMade { scenarioId = id } scenarioSelection



-- CREATE SCENARIO


createScenarioMutation : SelectionSet Id RootMutation
createScenarioMutation =
    Mutation.createScenario id


createScenario : Backend -> (Result (Graphql.Http.Error Id) Id -> msg) -> Cmd msg
createScenario backend msg =
    Backend.sendAuthorizedMutation backend createScenarioMutation msg
