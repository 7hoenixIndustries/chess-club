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
import Chess.MetaGame as Chess
import Graphql.Document
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Lazy exposing (..)
import Js
import Json.Decode
import Page.Learn.Scenario as Scenario exposing (Move, scenarioSelection, subscribeToMoves)
import Prelude
import Session
import Skeleton



-- MODEL


type alias Model =
    { chessModel : Maybe Chess.Model
    , session : Session.Data
    , scenarios : Scenarios
    , subscriptionStatus : SubscriptionStatus
    , scenario : Maybe Scenario.Scenario
    }


type SubscriptionStatus
    = Connected
    | NotConnected
    | Reconnecting


type Scenarios
    = Failure
    | Loading
    | Success (List Scenario.Scenario2)


init : Backend -> Session.Data -> ( Model, Cmd Msg )
init backend session =
    case Session.getScenarios session of
        Just entries ->
            ( Model Nothing session (Success entries) NotConnected Nothing
            , Cmd.none
            )

        Nothing ->
            ( Model Nothing session Loading NotConnected Nothing
            , Cmd.batch
                [ Scenario.getScenarios backend GotScenarios

                --[ Scenario.getScenario backend (Api.Scalar.Id "1") GotScenario
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
    | SentMove (Result (Graphql.Http.Error ()) ())
    | SubscriptionDataReceived Json.Decode.Value


update : Backend -> Msg -> Model -> ( Model, Cmd Msg )
update backend msg model =
    case msg of
        ChessMsg chessMsg ->
            case model.chessModel of
                Nothing ->
                    ( model, Cmd.none )

                Just chessModel ->
                    stepChess model (Chess.update (Chess.Callbacks MakeMove ChessMsg) chessMsg chessModel)

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
                        | scenarios = Success (List.map Scenario.Seed scenarios)
                        , session = Session.addScenarios (List.map Scenario.Seed scenarios) model.session
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
                    let
                        ( chessModel, chessMsg ) =
                            Chess.init scenario.availableMoves scenario.currentState scenario.recentMove ChessMsg
                    in
                    -- TODO: chessModel is directly dependent on scenario. . . are we able to combine these somehow?
                    ( { model
                        | scenario = Just scenario
                        , chessModel = Just chessModel
                      }
                    , Cmd.batch
                        [ Js.createSubscriptions (subscribeToMoves scenario.id |> Graphql.Document.serializeSubscription)
                        , chessMsg
                        ]
                    )

        MakeMove move ->
            case model.scenario of
                Just scenario ->
                    ( model
                    , Scenario.makeMove backend scenario.id move SentMove
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
                            ( { model | scenarios = Success <| scenarios ++ [ Scenario.Seed <| Scenario.ScenarioSeed id ] }, Scenario.getScenario backend id GotScenario )

                        -- This state should not be possible (assuming we aren't able to click the create button unless we are loaded.
                        _ ->
                            ( model
                            , Cmd.none
                            )

        SentMove _ ->
            ( model, Cmd.none )

        SubscriptionDataReceived newData ->
            case model.scenario of
                Just scenario ->
                    case Json.Decode.decodeValue (subscribeToMoves scenario.id |> Graphql.Document.decoder) newData of
                        Ok s ->
                            ( { model
                                | scenario = Just s
                                , chessModel = Maybe.map (\chessModel -> { chessModel | game = Chess.makeGame s.availableMoves s.currentState s.recentMove }) model.chessModel
                              }
                            , Cmd.none
                            )

                        Err error ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


stepChess : Model -> ( Chess.Model, Cmd Msg ) -> ( Model, Cmd Msg )
stepChess model ( chessModel, chessCmds ) =
    ( { model | chessModel = Just chessModel }, chessCmds )



-- VIEW


view : Model -> Skeleton.Details Msg
view model =
    { title = "Learn"
    , header = []
    , warning = Skeleton.NoProblems
    , attrs = [ class "container mx-auto px-4" ]
    , children =
        [ lazy3 viewLearn model.scenario model.chessModel model.subscriptionStatus
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
                        ++ List.map (lazy viewSelectScenario) ss
                    )
        ]


viewSelectScenario : Scenario.Scenario2 -> Html Msg
viewSelectScenario scenario =
    case scenario of
        Scenario.Seed { id } ->
            let
                (Api.Scalar.Id raw) =
                    id
            in
            div []
                [ button [ onClick (GetScenario id) ] [ text raw ]
                ]

        Scenario.Loaded s ->
            let
                (Api.Scalar.Id raw) =
                    s.id
            in
            div []
                [ button [ onClick (GotScenario (Ok s)) ] [ text raw ]
                ]



-- VIEW LEARN


viewLearn : Maybe Scenario.Scenario -> Maybe Chess.Model -> SubscriptionStatus -> Html Msg
viewLearn scenario chessModel subscriptionStatus =
    div [ class "container mx-auto flex items-center rounded-xl " ]
        [ case ( scenario, chessModel ) of
            ( Just s, Just c ) ->
                lazy3 viewScenario s c subscriptionStatus

            ( Nothing, Just _ ) ->
                div [] [ text "Scenario not loaded." ]

            ( Just _, Nothing ) ->
                div [] [ text "Chess not loaded." ]

            ( Nothing, Nothing ) ->
                div [] []
        ]



-- VIEW SCENARIO


viewScenario : Scenario.Scenario -> Chess.Model -> SubscriptionStatus -> Html Msg
viewScenario scenario chessModel subscriptionStatus =
    div [ class "container mx-auto" ]
        [ viewConnection subscriptionStatus
        , Html.map ChessMsg (lazy Chess.view chessModel)
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
        , Prelude.maybe Sub.none (\chessModel -> Sub.map ChessMsg (Chess.subscriptions chessModel)) model.chessModel
        ]
