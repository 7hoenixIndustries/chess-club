module Page.LearnTest exposing (all, start)

import Api.Scalar exposing (Id(..))
import Backend exposing (Backend)
import Chess.Position as Position exposing (Position)
import Page.Learn as Learn
import Page.Learn.Scenario as Scenario exposing (Fen(..), PreviousMovesSafe(..))
import Prelude
import ProgramTest exposing (ProgramTest, clickButton, ensureViewHas, ensureViewHasNot, expectViewHas, fillIn, simulateDomEvent, update)
import Skeleton
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (text)


backend : Backend
backend =
    Backend.api "http://foo.bar" "some-auth-token"


loadedData =
    { scenarios = Just [ Scenario.Loaded <| Scenario.Scenario [] (Fen "8/8/8/8/2p5/r3k3/8/R3K3 b - - 1 77") (PreviousMovesSafe []) (Id "1") ]
    }


start : ProgramTest Learn.Model Learn.Msg (Cmd Learn.Msg)
start =
    ProgramTest.createDocument
        { init = \_ -> Learn.init backend loadedData
        , view = \model -> Skeleton.view backend (\msg -> msg) (Learn.view model)
        , update = Learn.update backend
        }
        |> ProgramTest.start ()


all : Test
all =
    describe "Learn page"
        [ test "shows trivial" <|
            \() ->
                start
                    |> clickButton "1"
                    --|> ensureViewHasNot [ Selector.attribute <| Prelude.dataId "reinforcing-from" "c4" ]
                    --|> mouseDownOnSquare Position.a3
                    |> expectViewHas [ text "1" ]
        ]



--mouseDownOnSquare : Position ->
-- TODO: Elm Progam Test reports that we do not have a 'pageX' on this object when we try to grab it.


mouseDownOnSquare position =
    simulateDomEvent (Query.find [ Selector.class (Position.toAlgebraic position) ]) Event.mouseDown


mouseOverSquare position =
    simulateDomEvent (Query.find [ Selector.class position ]) Event.mouseOver


mouseUpOnSquare position =
    simulateDomEvent (Query.find [ Selector.class position ]) Event.mouseUp
