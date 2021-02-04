module Page.Learn exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Api.Scalar exposing (Id)
import Backend exposing (Backend)
import Chess.Game as Chess
import Chess.Search as Search
import Graphql.Document
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Lazy exposing (..)
import Js
import Json.Decode
import Page.Learn.Scenario as Scenario exposing (Move, scenarioSelection, subscribeToMoves)
import Session
import Skeleton



-- MODEL


type alias Model =
    { session : Session.Data
    , scenarios : Scenarios
    , subscriptionStatus : SubscriptionStatus
    , scenarioStuff : Maybe ScenarioStuff
    }


type alias ScenarioStuff =
    { scenario : Scenario.Scenario, chessModel : Chess.Model, searchModel : Search.Model }


type SubscriptionStatus
    = Connected
    | NotConnected
    | Reconnecting


type Scenarios
    = Failure
    | Loading
    | Success (List Scenario.ScenarioSeed)


init : Backend -> Session.Data -> ( Model, Cmd Msg )
init backend session =
    case Session.getScenarios session of
        Just entries ->
            ( Model session (Success entries) NotConnected Nothing
            , Cmd.none
            )

        Nothing ->
            ( Model session Loading NotConnected Nothing
            , Cmd.batch
                [ Scenario.getScenarios backend GotScenarios
                ]
            )



-- UPDATE


type Msg
    = ChessMsg Chess.Msg
    | CreateScenario
    | GetScenario Api.Scalar.Id
    | GotScenarios (Result (Graphql.Http.Error (List Scenario.ScenarioSeed)) (List Scenario.ScenarioSeed))
    | GotScenario (Result (Graphql.Http.Error Scenario.Scenario) Scenario.Scenario)
    | MakeMove Move
    | NewSubscriptionStatus SubscriptionStatus ()
    | ScenarioCreated (Result (Graphql.Http.Error Id) Id)
    | SearchMsg Search.Msg
    | SentMove (Result (Graphql.Http.Error ()) ())
    | SubscriptionDataReceived Json.Decode.Value


update : Backend -> Msg -> Model -> ( Model, Cmd Msg )
update backend msg model =
    case msg of
        ChessMsg chessMsg ->
            case model.scenarioStuff of
                Nothing ->
                    ( model, Cmd.none )

                Just scenarioStuff ->
                    stepChess model (Chess.update (Chess.Callbacks MakeMove) chessMsg scenarioStuff.chessModel)

        CreateScenario ->
            ( model, Scenario.createScenario backend ScenarioCreated )

        GetScenario id ->
            ( model, Scenario.getScenario backend id GotScenario )

        GotScenarios result ->
            case result of
                Err _ ->
                    ( { model | scenarios = Failure }
                    , Cmd.none
                    )

                Ok scenarios ->
                    ( { model
                        | scenarios = Success scenarios
                        , session = Session.addScenarios scenarios model.session
                      }
                    , Cmd.none
                    )

        GotScenario result ->
            case result of
                Err _ ->
                    ( model
                    , Cmd.none
                    )

                Ok scenario ->
                    -- TODO: chessModel is directly dependent on scenario. . . are we able to combine these somehow?
                    let
                        ( chessModel, chessMsgs ) =
                            Chess.init scenario.availableMoves scenario.currentState

                        ( searchModel, searchMsgs ) =
                            Search.init chessModel.game
                    in
                    ( { model
                        | scenarioStuff = Just <| ScenarioStuff scenario chessModel searchModel
                      }
                    , Cmd.batch
                        [ Js.createSubscriptions (subscribeToMoves scenario.id |> Graphql.Document.serializeSubscription)
                        , Cmd.map ChessMsg chessMsgs
                        , Cmd.map SearchMsg searchMsgs
                        ]
                    )

        MakeMove move ->
            case model.scenarioStuff of
                Just scenarioStuff ->
                    -- Hey there demeter. . . what's up with your law?
                    ( model
                    , Scenario.makeMove backend scenarioStuff.scenario.id move SentMove
                    )

                Nothing ->
                    ( model, Cmd.none )

        NewSubscriptionStatus status () ->
            ( { model | subscriptionStatus = status }, Cmd.none )

        ScenarioCreated result ->
            case result of
                Err _ ->
                    ( model
                    , Cmd.none
                    )

                Ok id ->
                    case model.scenarios of
                        Success scenarios ->
                            ( { model | scenarios = Success <| scenarios ++ [ Scenario.ScenarioSeed id ] }, Scenario.getScenario backend id GotScenario )

                        -- This state should not be possible (assuming we aren't able to click the create button unless we are loaded.
                        _ ->
                            ( model
                            , Cmd.none
                            )

        SearchMsg searchMsg ->
            case model.scenarioStuff of
                Nothing ->
                    ( model, Cmd.none )

                Just scenarioStuff ->
                    stepSearch model (Search.update searchMsg scenarioStuff.searchModel)

        SentMove _ ->
            ( model, Cmd.none )

        SubscriptionDataReceived newData ->
            case model.scenarioStuff of
                Just scenarioStuff ->
                    case Json.Decode.decodeValue (subscribeToMoves scenarioStuff.scenario.id |> Graphql.Document.decoder) newData of
                        Ok s ->
                            let
                                chessModelToUpdate =
                                    scenarioStuff.chessModel

                                game =
                                    Chess.makeGame s.availableMoves s.currentState
                            in
                            ( { model
                                | scenarioStuff = Just <| ScenarioStuff s { chessModelToUpdate | game = game } scenarioStuff.searchModel
                              }
                            , Cmd.none
                            )

                        Err error ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


stepChess : Model -> ( Chess.Model, Cmd Msg ) -> ( Model, Cmd Msg )
stepChess model ( chessModel, chessCmds ) =
    ( { model | scenarioStuff = Maybe.map (\scenarioStuff -> { scenarioStuff | chessModel = chessModel }) model.scenarioStuff }
    , chessCmds
    )


stepSearch : Model -> ( Search.Model, Cmd Search.Msg ) -> ( Model, Cmd Msg )
stepSearch model ( searchModel, searchCmds ) =
    ( { model | scenarioStuff = Maybe.map (\scenarioStuff -> { scenarioStuff | searchModel = searchModel }) model.scenarioStuff }
    , Cmd.map SearchMsg searchCmds
    )



-- VIEW


view : Model -> Skeleton.Details Msg
view model =
    { title = "Learn"
    , header = []
    , warning = Skeleton.NoProblems
    , attrs = [ class "container mx-auto px-4" ]
    , children =
        [ lazy2 viewLearn model.scenarioStuff model.subscriptionStatus
        , lazy viewScenarios model.scenarios
        ]
    }


viewConnection : SubscriptionStatus -> Html Msg
viewConnection status =
    case status of
        Connected ->
            div [ class "rounded-full bg-green-500 w-4 h-4" ] []

        Reconnecting ->
            div [ class "rounded-full bg-yellow-500 w-4 h-4" ] []

        NotConnected ->
            div [ class "rounded-full bg-red-500 w-4 h-4" ] []


viewScenarios : Scenarios -> Html Msg
viewScenarios scenarios =
    div [ classList [ ( "scenarios", True ), ( "p-6 max-w-sm mx-auto bg-white rounded-xl shadow-md flex justify-end items-center space-x-4", True ) ] ]
        [ case scenarios of
            Failure ->
                div [] []

            --div Problem.styles (Problem.offline "scenarios.json")
            Loading ->
                text ""

            Success [] ->
                div [ class "flex-shrink-0 text-xl font-medium text-purple-600" ]
                    [ button [ onClick CreateScenario ] [ text "Create Scenario" ]
                    , text "You don't seem to have any scenarios."
                    ]

            Success ss ->
                div []
                    ([ button [ onClick CreateScenario ] [ text "Create Scenario" ]
                     ]
                        ++ List.map viewSelectScenario ss
                    )
        ]


viewSelectScenario : Scenario.ScenarioSeed -> Html Msg
viewSelectScenario { id } =
    let
        (Api.Scalar.Id raw) =
            id
    in
    div []
        [ button [ onClick (GetScenario id) ] [ text raw ]
        ]



-- VIEW LEARN


viewLearn : Maybe ScenarioStuff -> SubscriptionStatus -> Html Msg
viewLearn scenarioStuff subscriptionStatus =
    div [ class "p-6 max-w-sm mx-auto bg-white rounded-xl shadow-md flex items-center space-x-4" ]
        [ case scenarioStuff of
            Just stuff ->
                viewScenario stuff subscriptionStatus

            Nothing ->
                div [] [ text "Scenario not loaded." ]
        ]



-- VIEW SCENARIO


viewScenario : ScenarioStuff -> SubscriptionStatus -> Html Msg
viewScenario scenarioStuff subscriptionStatus =
    div [ class "container flex flex-col mx-auto px-4" ]
        [ viewConnection subscriptionStatus
        , Html.map SearchMsg (Search.view scenarioStuff.searchModel)
        , Html.map ChessMsg (Chess.view scenarioStuff.chessModel)
        ]


backgroundColor : String -> String
backgroundColor color =
    if color == "b" then
        "bg-blue-500"

    else
        "bg-red-500"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Js.gotSubscriptionData SubscriptionDataReceived
        , Js.socketStatusConnected (NewSubscriptionStatus Connected)
        , Js.socketStatusReconnecting (NewSubscriptionStatus Reconnecting)
        ]
