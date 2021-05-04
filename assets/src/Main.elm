module Main exposing (main)

import Backend exposing (Backend)
import Browser
import Browser.Navigation as Nav
import Page.Learn as Learn
import Page.Problem as Problem
import Session
import Skeleton
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


type alias Model =
    { key : Nav.Key
    , page : Page
    , backend : Backend
    , settings : Settings
    }


type Settings
    = Settings { navbarOpen : Bool }


type Page
    = NotFound Session.Data
    | Learn Session.Data Learn.Model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Learn _ learnModel ->
            Sub.map LearnMsg <| Learn.subscriptions learnModel

        NotFound _ ->
            Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        NotFound _ ->
            Skeleton.view (Skeleton.Callbacks SetNavbarOpen)
                model.backend
                never
                { title = "Not Found"
                , navbarOpen = True
                , header = []
                , warning = Skeleton.NoProblems
                , attrs = Problem.styles
                , children = Problem.notFound
                }

        Learn _ learn ->
            Skeleton.view (Skeleton.Callbacks SetNavbarOpen) model.backend LearnMsg (Learn.view learn)



-- INIT


type alias Flags =
    { backendEndpoint : String
    , authToken : String
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init { backendEndpoint, authToken } url key =
    stepUrl url
        { key = key
        , page = NotFound Session.empty
        , backend = Backend.api backendEndpoint authToken
        , settings = Settings { navbarOpen = True }
        }



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | LearnMsg Learn.Msg
    | SetNavbarOpen Bool
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            stepUrl url model

        SetNavbarOpen navbarOpen ->
            ( { model | settings = Settings { navbarOpen = navbarOpen } }, Cmd.none )

        LearnMsg msg ->
            case model.page of
                Learn _ learn ->
                    stepLearn model (Learn.update model.backend msg learn)

                NotFound _ ->
                    ( model, Cmd.none )


stepLearn : Model -> ( Learn.Model, Cmd Learn.Msg ) -> ( Model, Cmd Msg )
stepLearn model ( learn, cmds ) =
    ( { model | page = Learn (extractSession model.page) learn }
    , Cmd.map LearnMsg cmds
    )



-- EXIT


exit : Model -> Session.Data
exit model =
    case model.page of
        NotFound session ->
            session

        Learn session _ ->
            session



-- ROUTER


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        session =
            exit model

        parser =
            oneOf
                [ route (s "app")
                    (stepLearn model (Learn.init Learn.Join model.backend session))
                ]
    in
    case Parser.parse parser url of
        Just answer ->
            answer

        Nothing ->
            ( { model | page = NotFound session }
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
