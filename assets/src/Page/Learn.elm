module Page.Learn exposing
    ( Configuration(..)
    , Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

{-| Configuration

This module supports two different flows.

They are able to switch between each other based on what the user actually chooses,
but you get to pick which one to start with. <- this is handy for if you want to be able
to help someone get right back to where they were if they are ever disconnected.

The two flows are joining & learning.

  - Joining means that you are looking through available scenarios aiming to choose one.
      - Current numbers on each team changes percentage likelihood to join each team.
  - Learning means that you are subscribing to a scenario.
      - Currently, and (perhaps always will only) support(s) one at a time.

-}

import Api.Scalar exposing (Id)
import Backend exposing (Backend)
import Chess.Logic as Logic
import Chess.MetaGame as Chess
import Graphql.Document
import Graphql.Http
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Lazy exposing (..)
import Js
import Json.Decode
import Page.Learn.Scenario as Scenario exposing (Fen(..), Move, scenarioSelection, subscribeToMoves)
import Prelude
import Responsive exposing (Responsive)
import Session
import Set exposing (Set)
import Skeleton
import Vote.Main


{-| Configuration

joining : Joining
joining =
Joining Set.empty Set.empty

learning : Learning
learning =
Learning Internal

-}
type Configuration
    = Join
    | Learn



-- MODEL


type Model
    = Learning Internal
    | Joining Logic.Model (JoinOdds Membership)


type JoinOdds membership
    = JoinOdds membership


type Membership
    = Membership TeamThatGoesFirst TeamThatGoesSecond


type TeamThatGoesFirst
    = TeamThatGoesFirst (Set Player)


type TeamThatGoesSecond
    = TeamThatGoesSecond (Set Player)


type Player
    = Player Settings


{-| This is where we plug in the Livery section.

type Settings = Settings Livery

-}
type Settings
    = Settings


type alias Internal =
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


init : Configuration -> Backend -> Session.Data -> ( Model, Cmd Msg )
init configuration backend session =
    case configuration of
        Join ->
            case Logic.init2 (Fen "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2") of
                Ok gameModel ->
                    ( Joining gameModel (JoinOdds (Membership (TeamThatGoesFirst Set.empty) (TeamThatGoesSecond Set.empty))), Cmd.none )

                Err bad ->
                    --( Joining (JoinOdds (Membership (TeamThatGoesFirst Set.empty) (TeamThatGoesSecond Set.empty))), Cmd.none )
                    Debug.todo "Debug failed from bad game state."

        Learn ->
            case Session.getScenarios session of
                Just entries ->
                    ( Learning <| Internal Nothing session (Success entries) NotConnected Nothing
                    , Cmd.none
                    )

                Nothing ->
                    let
                        runSpecific =
                            --Debug.log "Ensuring not possible to release" (Just "17")
                            Nothing
                    in
                    ( Learning <| Internal Nothing session Loading NotConnected Nothing
                    , Cmd.batch
                        [ case runSpecific of
                            Nothing ->
                                Scenario.getScenarios backend GotScenarios

                            Just id ->
                                Scenario.getScenario backend (Api.Scalar.Id id) GotScenario
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
    case model of
        Learning internal ->
            upateLearning backend msg internal

        Joining _ (JoinOdds membership) ->
            ( model, Cmd.none )


upateLearning backend msg model =
    case msg of
        ChessMsg chessMsg ->
            case model.chessModel of
                Nothing ->
                    ( Learning <| model, Cmd.none )

                Just chessModel ->
                    stepChess model (Chess.update (Chess.Callbacks MakeMove ChessMsg) chessMsg chessModel)
                        |> Tuple.mapFirst Learning

        --VoteMsg chessMsg ->
        --    case model.chessModel of
        --        Nothing ->
        --            ( model, Cmd.none )
        --
        --        Just chessModel ->
        --            stepChess model (Chess.update (Chess.Callbacks MakeMove ChessMsg) chessMsg chessModel)
        -- Scenario Stuff
        CreateScenario ->
            ( model, Scenario.createScenario backend ScenarioCreated )
                |> Tuple.mapFirst Learning

        GetScenario id ->
            ( model, Scenario.getScenario backend id GotScenario )
                |> Tuple.mapFirst Learning

        GotScenarios result ->
            case result of
                Err _ ->
                    ( { model | scenarios = Failure }
                    , Cmd.none
                    )
                        |> Tuple.mapFirst Learning

                Ok scenarios ->
                    ( { model
                        | scenarios = Success (List.map Scenario.Seed scenarios)
                        , session = Session.addScenarios (List.map Scenario.Seed scenarios) model.session
                      }
                    , Cmd.none
                    )
                        |> Tuple.mapFirst Learning

        GotScenario result ->
            case result of
                Err _ ->
                    ( model
                    , Cmd.none
                    )
                        |> Tuple.mapFirst Learning

                Ok scenario ->
                    let
                        ( chessModel, chessMsg ) =
                            Chess.init scenario.availableMoves scenario.currentState scenario.previousMoves ChessMsg
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
                        |> Tuple.mapFirst Learning

        MakeMove move ->
            case model.scenario of
                Just scenario ->
                    ( model
                    , Scenario.makeMove backend scenario.id move SentMove
                    )
                        |> Tuple.mapFirst Learning

                Nothing ->
                    ( model, Cmd.none )
                        |> Tuple.mapFirst Learning

        NewSubscriptionStatus status () ->
            ( { model | subscriptionStatus = status }, Cmd.none )
                |> Tuple.mapFirst Learning

        ScenarioCreated result ->
            case result of
                Err _ ->
                    ( model
                    , Cmd.none
                    )
                        |> Tuple.mapFirst Learning

                Ok id ->
                    case model.scenarios of
                        Success scenarios ->
                            ( { model | scenarios = Success <| scenarios ++ [ Scenario.Seed <| Scenario.ScenarioSeed id ] }, Scenario.getScenario backend id GotScenario )
                                |> Tuple.mapFirst Learning

                        -- This state should not be possible (assuming we aren't able to click the create button unless we are loaded.
                        _ ->
                            ( model
                            , Cmd.none
                            )
                                |> Tuple.mapFirst Learning

        SentMove _ ->
            ( model, Cmd.none )
                |> Tuple.mapFirst Learning

        SubscriptionDataReceived newData ->
            case model.scenario of
                Just scenario ->
                    case Json.Decode.decodeValue (subscribeToMoves scenario.id |> Graphql.Document.decoder) newData of
                        Ok s ->
                            case model.chessModel of
                                Nothing ->
                                    ( { model
                                        | scenario = Just s
                                        , chessModel = Nothing
                                      }
                                    , Cmd.none
                                    )
                                        |> Tuple.mapFirst Learning

                                Just chessModel ->
                                    let
                                        ( updatedModel, chessMsgs ) =
                                            Chess.moveMade s.availableMoves s.previousMoves ChessMsg chessModel
                                    in
                                    ( { model
                                        | scenario = Just s
                                        , chessModel = Just updatedModel
                                      }
                                    , chessMsgs
                                    )
                                        |> Tuple.mapFirst Learning

                        Err error ->
                            -- TODO: Display error so they know to refresh.
                            ( model, Cmd.none )
                                |> Tuple.mapFirst Learning

                Nothing ->
                    ( model, Cmd.none )
                        |> Tuple.mapFirst Learning


stepChess : Internal -> ( Chess.Model, Cmd Msg ) -> ( Internal, Cmd Msg )
stepChess model ( chessModel, chessCmds ) =
    ( { model | chessModel = Just chessModel }, chessCmds )



-- VIEW


view : Responsive -> Model -> Skeleton.Details Msg
view responsive model =
    Skeleton.Details
        { title = "Learn"
        , navbarOpen = False
        , header = []
        , warning = Skeleton.NoProblems
        , attrs = [ class "container mx-auto px-4" ]
        , children =
            case model of
                Joining chessState internal ->
                    [ Vote.Main.view responsive chessState
                    ]

                Learning internal ->
                    [ lazy3 viewLearn internal.scenario internal.chessModel internal.subscriptionStatus
                    , lazy viewScenarios internal.scenarios
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

        --, makeTreeShakingHappy
        ]


makeTreeShakingHappy : Html msg
makeTreeShakingHappy =
    div
        [ classList
            [ ( "translate-x-0-squares", True )
            , ( "translate-x-1-squares", True )
            , ( "translate-x-2-squares", True )
            , ( "translate-x-3-squares", True )
            , ( "translate-x-4-squares", True )
            , ( "translate-x-5-squares", True )
            , ( "translate-x-6-squares", True )
            , ( "translate-x-7-squares", True )
            , ( "translate-x-neg-0-squares", True )
            , ( "translate-x-neg-1-squares", True )
            , ( "translate-x-neg-2-squares", True )
            , ( "translate-x-neg-3-squares", True )
            , ( "translate-x-neg-4-squares", True )
            , ( "translate-x-neg-5-squares", True )
            , ( "translate-x-neg-6-squares", True )
            , ( "translate-x-neg-7-squares", True )
            , ( "translate-y-0-squares", True )
            , ( "translate-y-1-squares", True )
            , ( "translate-y-2-squares", True )
            , ( "translate-y-3-squares", True )
            , ( "translate-y-4-squares", True )
            , ( "translate-y-5-squares", True )
            , ( "translate-y-6-squares", True )
            , ( "translate-y-7-squares", True )
            , ( "translate-y-neg-0-squares", True )
            , ( "translate-y-neg-1-squares", True )
            , ( "translate-y-neg-2-squares", True )
            , ( "translate-y-neg-3-squares", True )
            , ( "translate-y-neg-4-squares", True )
            , ( "translate-y-neg-5-squares", True )
            , ( "translate-y-neg-6-squares", True )
            , ( "translate-y-neg-7-squares", True )
            ]
        ]
        []


backgroundColor : String -> String
backgroundColor color =
    if color == "b" then
        "bg-blue-500"

    else
        "bg-red-500"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Learning internal ->
            Sub.batch
                [ Js.gotSubscriptionData SubscriptionDataReceived
                , Js.socketStatusConnected (NewSubscriptionStatus Connected)
                , Js.socketStatusReconnecting (NewSubscriptionStatus Reconnecting)
                , Prelude.maybe Sub.none (\chessModel -> Sub.map ChessMsg (Chess.subscriptions chessModel)) internal.chessModel
                ]

        Joining _ (JoinOdds membership) ->
            Sub.none
