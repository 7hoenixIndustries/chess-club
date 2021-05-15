module Main exposing (main)

import Backend exposing (Backend)
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Page.Learn as Learn
import Page.Problem as Problem
import Responsive exposing (Responsive)
import Session
import Skeleton exposing (Details(..))
import Task
import Url
import Url.Parser as Parser exposing ((</>), Parser, custom, fragment, map, oneOf, s, top)



-- MAIN


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


type Model
    = Model
        Shell
        { key : Nav.Key
        , page : Page
        , backend : Backend
        , settings : Settings
        }


type Shell
    = Shell Responsive


type Settings
    = Settings { navbarOpen : Bool }


type Page
    = NotFound Session.Data
    | Learn Session.Data Learn.Model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions (Model _ model) =
    Sub.batch
        [ case model.page of
            Learn _ learnModel ->
                Sub.map LearnMsg <| Learn.subscriptions learnModel

            NotFound _ ->
                Sub.none
        , Browser.Events.onResize WindowResized
        ]



-- VIEW


view : Model -> Browser.Document Msg
view (Model (Shell responsive) model) =
    case model.page of
        NotFound _ ->
            Skeleton.view
                (Skeleton.Callbacks SetNavbarOpen)
                model.backend
                never
                (Details
                    { title = "Not Found"
                    , navbarOpen = True
                    , header = []
                    , warning = Skeleton.NoProblems
                    , attrs = Problem.styles
                    , children = Problem.notFound
                    }
                )

        Learn _ learn ->
            Skeleton.view (Skeleton.Callbacks SetNavbarOpen) model.backend LearnMsg (Learn.view responsive learn)



-- INIT


type alias Flags =
    { backendEndpoint : String
    , authToken : String
    , width : Int
    , height : Int
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init { backendEndpoint, authToken, width, height } url key =
    stepUrl url
        (Model
            (Shell <| Responsive.init { width = width, height = height })
            { key = key
            , page = NotFound Session.empty
            , backend = Backend.api backendEndpoint authToken
            , settings = Settings { navbarOpen = True }
            }
        )
        |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, getBrowserWindow ])



-- UPDATE


type Msg
    = GotBrowserElement (Result Browser.Dom.Error Browser.Dom.Viewport)
    | LinkClicked Browser.UrlRequest
    | LearnMsg Learn.Msg
    | SetNavbarOpen Bool
    | UrlChanged Url.Url
    | WindowResized Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update message (Model shell model) =
    case message of
        GotBrowserElement result ->
            case result of
                Err failed ->
                    let
                        hi =
                            Debug.log "failed" failed
                    in
                    ( Model shell model, Cmd.none )

                Ok mainNode ->
                    let
                        hi =
                            Debug.log "mainNod" mainNode
                    in
                    ( Model shell model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( Model shell model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( Model shell model
                    , Nav.load href
                    )

        UrlChanged url ->
            stepUrl url (Model shell model)

        SetNavbarOpen navbarOpen ->
            ( Model shell { model | settings = Settings { navbarOpen = navbarOpen } }, Cmd.none )

        LearnMsg msg ->
            case model.page of
                Learn _ learn ->
                    stepLearn (Model shell model) (Learn.update model.backend msg learn)

                NotFound _ ->
                    ( Model shell model, Cmd.none )

        WindowResized width height ->
            ( Model (Shell <| Responsive.init { width = width, height = height }) model, Cmd.none )


stepLearn : Model -> ( Learn.Model, Cmd Learn.Msg ) -> ( Model, Cmd Msg )
stepLearn (Model shell model) ( learn, cmds ) =
    ( Model shell { model | page = Learn (extractSession model.page) learn }
    , Cmd.map LearnMsg cmds
    )



-- EXIT


exit : Model -> Session.Data
exit (Model shell model) =
    case model.page of
        NotFound session ->
            session

        Learn session _ ->
            session



-- ROUTER


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url (Model ((Shell responsive) as shell) model) =
    let
        session =
            exit (Model shell model)

        parser =
            oneOf
                [ route (s "app")
                    (stepLearn (Model shell model) (Learn.init Learn.Join model.backend session))
                ]
    in
    case Parser.parse parser url of
        Just answer ->
            answer

        Nothing ->
            ( Model shell { model | page = NotFound session }
            , Cmd.none
            )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser


extractSession page =
    case page of
        NotFound session ->
            session

        Learn session _ ->
            session


getBrowserWindow : Cmd Msg
getBrowserWindow =
    Task.attempt GotBrowserElement (Browser.Dom.getViewportOf "main")
