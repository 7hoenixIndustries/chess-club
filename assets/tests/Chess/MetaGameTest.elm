module Chess.MetaGameTest exposing (all)

import Chess.Logic as Logic
import Chess.MetaGame as MetaGame
import Chess.Position as Position
import Dict
import Expect
import Page.Learn.Scenario exposing (Move)
import Test exposing (Test, describe, test)


blackMove : Move
blackMove =
    { fenAfterMove = ""
    , squareFrom = ""
    , squareTo = ""
    , color = "b"
    , moveCommand = ""
    }


all : Test
all =
    describe "MetaGame"
        [ describe "reinforcingSquares" <|
            [ describe "when position is a legal move" <|
                [ test "returns the positions given" <|
                    \() ->
                        Dict.fromList [ ( Position.toAlgebraic Position.a1 ++ Position.toAlgebraic Position.a2, blackMove ) ]
                            |> MetaGame.reinforcingSquares { starting = Position.a1, current = Position.a2 } [ Position.a2 ]
                            |> Expect.equal [ Position.a2 ]
                ]
            , describe "when position is not a legal move" <|
                [ test "returns no reinforcingMoves" <|
                    \() ->
                        MetaGame.reinforcingSquares { starting = Position.a1, current = Position.a2 } [ Position.a2 ] Dict.empty
                            |> Expect.equal []
                ]
            ]
        ]
