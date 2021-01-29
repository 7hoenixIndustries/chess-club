module Chess.GameTest exposing (all)

import Chess.Board as Position exposing (Position(..))
import Chess.Game as Game exposing (Piece, PieceType(..), Square)
import Chess.Helpers exposing (..)
import Expect exposing (..)
import Fuzz exposing (Fuzzer)
import Test exposing (..)


diagonalMovesFromD4 =
    [ -- North East
      Position.c3
    , Position.b2
    , Position.a1

    -- South East
    , Position.c5
    , Position.b6
    , Position.a7

    -- North West
    , Position.e3
    , Position.f2
    , Position.g1

    -- South West
    , Position.e5
    , Position.f6
    , Position.g7
    , Position.h8
    ]


horizontalMovesFromD4 =
    [ -- North
      Position.d3
    , Position.d2
    , Position.d1

    -- South
    , Position.d5
    , Position.d6
    , Position.d7
    , Position.d8

    -- West
    , Position.e4
    , Position.f4
    , Position.g4
    , Position.h4

    -- East
    , Position.c4
    , Position.b4
    , Position.a4
    ]


pieceTypeFuzzer : Fuzzer PieceType
pieceTypeFuzzer =
    Fuzz.frequency
        [ ( 1, Fuzz.constant Advisor )
        , ( 1, Fuzz.constant Rook )
        , ( 1, Fuzz.constant Bishop )
        ]


position : Square -> ( Int, Int )
position (Game.Occupied ( px, py ) _) =
    ( px, py )



-- Temporary


toGamePosition : Position -> ( Int, Int )
toGamePosition (Position column row) =
    ( column, row )


all : Test
all =
    describe "Chess"
        [ describe "check mate" <|
            [ describe "knows if check is counterable (block, take, escape)" <|
                [ test "no blocks, takes, or escape leads to checkmate" <|
                    \() ->
                        let
                            current =
                                Game.Occupied (toGamePosition Position.h1) monarch

                            attackers =
                                [ Game.Occupied (toGamePosition Position.g8) opponentRook
                                , Game.Occupied (toGamePosition Position.h7) opponentRook
                                ]

                            game =
                                Game.initWithExistingState (attackers ++ [ current ]) (Game.Turn team)
                        in
                        Expect.true "Double rooks should be checkmating here." (Game.isCheckmate game)
                , test "if escapable then not checkmate" <|
                    \() ->
                        let
                            current =
                                Game.Occupied (toGamePosition Position.h1) monarch

                            attackers =
                                [ Game.Occupied (toGamePosition Position.h7) opponentRook
                                ]

                            game =
                                Game.initWithExistingState (attackers ++ [ current ]) (Game.Turn team)
                        in
                        Expect.false "Monarch may move out of check." (Game.isCheckmate game)
                , test "if blockable then not checkmate" <|
                    \() ->
                        let
                            current =
                                Game.Occupied (toGamePosition Position.h1) monarch

                            attackers =
                                [ Game.Occupied (toGamePosition Position.g8) opponentRook
                                , Game.Occupied (toGamePosition Position.h7) opponentRook
                                ]

                            blocker =
                                Game.Occupied (toGamePosition Position.a2) rook

                            game =
                                Game.initWithExistingState (attackers ++ [ current, blocker ]) (Game.Turn team)
                        in
                        Expect.false "Rook should be able to block." (Game.isCheckmate game)
                , test "if capturable then not checkmate" <|
                    \() ->
                        let
                            current =
                                Game.Occupied (toGamePosition Position.h1) monarch

                            attackers =
                                [ Game.Occupied (toGamePosition Position.g8) opponentRook
                                , Game.Occupied (toGamePosition Position.h3) opponentRook
                                ]

                            taker =
                                Game.Occupied (toGamePosition Position.f5) bishop

                            game =
                                Game.initWithExistingState (attackers ++ [ current, taker ]) (Game.Turn team)
                        in
                        Expect.false "Bishop should be able to capture the attacking Rook." (Game.isCheckmate game)
                ]
            ]
        , describe "check" <|
            [ describe "knows if monarch is under fire" <|
                [ test "knows if any opposing piece may move to the monarchs square" <|
                    \() ->
                        let
                            current =
                                Game.Occupied (toGamePosition Position.b2) monarch

                            attackers =
                                [ Game.Occupied (toGamePosition Position.b1) opponentRook
                                , Game.Occupied (toGamePosition Position.c1) opponentBishop
                                , Game.Occupied (toGamePosition Position.a1) opponentAdvisor
                                ]

                            opponentThatCantAttack =
                                Game.Occupied (toGamePosition Position.h1) opponentRook

                            sameTeam =
                                Game.Occupied (toGamePosition Position.b3) rook

                            game =
                                Game.initWithExistingState (attackers ++ [ current, opponentThatCantAttack, sameTeam ]) (Game.Turn team)
                        in
                        Expect.equal (List.sortBy position attackers) (List.sortBy position (Game.findChecks game))
                ]
            ]
        , describe "canMoveTo" <|
            [ describe "single piece" <|
                [ test "monarch movement" <|
                    \() ->
                        let
                            current =
                                Game.Occupied (toGamePosition Position.b2) monarch

                            game =
                                Game.initWithExistingState [ current ] (Game.Turn team)

                            validSquares =
                                [ toGamePosition Position.a1
                                , toGamePosition Position.a2
                                , toGamePosition Position.a3
                                , toGamePosition Position.b1
                                , toGamePosition Position.b3
                                , toGamePosition Position.c1
                                , toGamePosition Position.c2
                                , toGamePosition Position.c3
                                ]
                        in
                        Expect.true
                            "Knows monarch moves"
                            (List.all
                                (\pos -> Game.canMoveTo pos game == [ toGamePosition Position.b2 ])
                                validSquares
                            )
                , test "monarch invalid moves" <|
                    \() ->
                        let
                            current =
                                Game.Occupied (toGamePosition Position.b2) monarch

                            game =
                                Game.initWithExistingState [ current ] (Game.Turn team)

                            validSquares =
                                [ toGamePosition Position.a1
                                , toGamePosition Position.a2
                                , toGamePosition Position.a3
                                , toGamePosition Position.b1
                                , toGamePosition Position.b3
                                , toGamePosition Position.c1
                                , toGamePosition Position.c2
                                , toGamePosition Position.c3
                                ]

                            otherSquares =
                                List.filter (\s -> not (List.member s validSquares)) (List.map toGamePosition Position.all)
                        in
                        Expect.true
                            "Not valid monarch movement"
                            (List.all
                                (\pos -> Game.canMoveTo pos game == [])
                                otherSquares
                            )
                , test "bishop movement" <|
                    \() ->
                        let
                            current =
                                Game.Occupied (toGamePosition Position.d4) bishop

                            game =
                                Game.initWithExistingState [ current ] (Game.Turn team)
                        in
                        Expect.true
                            "Knows bishop moves"
                            (List.all
                                (\pos -> Game.canMoveTo pos game == [ toGamePosition Position.d4 ])
                                (List.map toGamePosition diagonalMovesFromD4)
                            )
                , test "bishop invalid moves" <|
                    \() ->
                        let
                            current =
                                Game.Occupied (toGamePosition Position.d4) bishop

                            game =
                                Game.initWithExistingState [ current ] (Game.Turn team)

                            otherSquares =
                                List.filter (\s -> not (List.member s diagonalMovesFromD4)) Position.all
                        in
                        Expect.true
                            "Not valid bishop movement"
                            (List.all
                                (\pos -> Game.canMoveTo pos game == [])
                                (List.map toGamePosition otherSquares)
                            )
                , test "rook movement" <|
                    \() ->
                        let
                            current =
                                Game.Occupied (toGamePosition Position.d4) rook

                            game =
                                Game.initWithExistingState [ current ] (Game.Turn team)
                        in
                        Expect.true
                            "Knows rook moves"
                            (List.all
                                (\pos -> Game.canMoveTo pos game == [ toGamePosition Position.d4 ])
                                (List.map toGamePosition horizontalMovesFromD4)
                            )
                , test "Rook invalid moves" <|
                    \() ->
                        let
                            current =
                                Game.Occupied (toGamePosition Position.d4) rook

                            game =
                                Game.initWithExistingState [ current ] (Game.Turn team)

                            otherSquares =
                                List.filter (\s -> not (List.member s horizontalMovesFromD4)) Position.all
                        in
                        Expect.true
                            "Not valid rook movement"
                            (List.all
                                (\pos -> Game.canMoveTo pos game == [])
                                (List.map toGamePosition otherSquares)
                            )
                , test "Advisor movement" <|
                    \() ->
                        let
                            current =
                                Game.Occupied (toGamePosition Position.d4) advisor

                            game =
                                Game.initWithExistingState [ current ] (Game.Turn team)
                        in
                        Expect.true
                            "Knows advisor moves"
                            (List.all
                                (\pos -> Game.canMoveTo pos game == [ toGamePosition Position.d4 ])
                                (List.map toGamePosition (horizontalMovesFromD4 ++ diagonalMovesFromD4))
                            )
                , test "Advisor invalid moves" <|
                    \() ->
                        let
                            current =
                                Game.Occupied (toGamePosition Position.d4) advisor

                            game =
                                Game.initWithExistingState [ current ] (Game.Turn team)

                            otherSquares =
                                List.filter
                                    (\s ->
                                        not
                                            (List.member s
                                                (horizontalMovesFromD4 ++ diagonalMovesFromD4)
                                            )
                                    )
                                    Position.all
                        in
                        Expect.equal []
                            (List.filter
                                (\pos -> Game.canMoveTo pos game /= [])
                                (List.map toGamePosition otherSquares)
                            )
                ]
            , describe "more pieces" <|
                [ test "movement not allowed if blocked by pieces" <|
                    \() ->
                        let
                            current =
                                Game.Occupied (toGamePosition Position.a1) advisor

                            friendly pos =
                                Game.Occupied pos rook

                            opponent pos =
                                Game.Occupied pos opponentRook

                            game =
                                Game.initWithExistingState [ friendly (toGamePosition Position.a2), current, friendly (toGamePosition Position.b1), opponent (toGamePosition Position.b2) ] (Game.Turn team)
                        in
                        Expect.equal [ toGamePosition Position.b2 ]
                            (List.filter
                                (\pos -> List.member (toGamePosition Position.a1) (Game.canMoveTo pos game))
                                (List.map toGamePosition Position.all)
                            )
                , test "restrict movement by turn" <|
                    \() ->
                        let
                            current =
                                Game.Occupied (toGamePosition Position.a1) opponentAdvisor

                            game =
                                Game.initWithExistingState [ current ] (Game.Turn team)
                        in
                        Expect.equal [] (Game.canMoveTo (toGamePosition Position.a2) game)
                , test "moves may not result in check" <|
                    \() ->
                        let
                            protected =
                                Game.Occupied (toGamePosition Position.a1) monarch

                            pinned =
                                Game.Occupied (toGamePosition Position.b1) advisor

                            pinner =
                                Game.Occupied (toGamePosition Position.c1) opponentAdvisor

                            game =
                                Game.initWithExistingState [ protected, pinned, pinner ] (Game.Turn team)
                        in
                        Expect.equal [] (Game.canMoveTo (toGamePosition Position.b3) game)
                ]
            ]
        ]
