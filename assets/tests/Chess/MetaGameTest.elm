module Chess.MetaGameTest exposing (all)

import Chess.Logic as Logic exposing (Piece)
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


move : String -> String -> Move
move team squareTo =
    { fenAfterMove = ""
    , squareFrom = ""
    , squareTo = squareTo
    , color = team
    , moveCommand = ""
    }



--type Owner
--    = YouOwn Breakdown
--    | TheyOwn Breakdown
--    | Occupied Piece
--
--type Breakdown = Breakdown { Dict Position Move, theirFolks : List Move }
--Dict Position Move


all : Test
all =
    describe "MetaGame"
        [ describe "centralSquares" <|
            [ describe "will show who owns what" <|
                [ test "shows no owners" <|
                    \() ->
                        Expect.equal (MetaGame.centralSquares Dict.empty)
                            { e5 = { us = 0, them = 0 }
                            , d5 = { us = 0, them = 0 }
                            , e4 = { us = 0, them = 0 }
                            , d4 = { us = 0, them = 0 }
                            }
                , test "shows a single owner with no contesting" <|
                    \() ->
                        let
                            moves =
                                [ move "b" "e5"
                                , move "b" "e5"
                                , move "b" "e4"
                                , move "w" "d5"
                                , move "b" "d5"
                                , move "b" "d5"
                                , move "w" "d4"
                                ]
                        in
                        Expect.equal (MetaGame.centralSquares (Logic.movesDict moves))
                            { e5 = { us = 2, them = 0 }
                            , d5 = { us = 2, them = 1 }
                            , e4 = { us = 1, them = 0 }
                            , d4 = { us = 0, them = 1 }
                            }

                --, test "attackers  need 1 more piece to own a square" <|
                --, test "occupied squares with no attackers against them count as being owned by the occupier" <|
                --, test "unoccupied squares . . ." <|
                ]
            ]
        , describe "reinforcingSquares" <|
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
