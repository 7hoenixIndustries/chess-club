module Chess.LogicTest exposing (all)

import Chess.Logic as Chess exposing (Piece, PieceType(..), Square)
import Chess.Position as Position exposing (Position(..))
import Dict
import Expect exposing (..)
import Fuzz exposing (Fuzzer)
import Test exposing (..)


team =
    Chess.Black


opponentTeam =
    Chess.White


monarch =
    Chess.Piece team Chess.Monarch


opponentMonarch =
    Chess.Piece opponentTeam Chess.Monarch


advisor =
    Chess.Piece team Chess.Advisor


opponentAdvisor =
    Chess.Piece opponentTeam Chess.Advisor


rook =
    Chess.Piece team Chess.Rook


opponentRook =
    Chess.Piece opponentTeam Chess.Rook


bishop =
    Chess.Piece team Chess.Bishop


knight =
    Chess.Piece team Chess.Knight


pawn =
    Chess.Piece team Chess.Pawn


opponentPawn =
    Chess.Piece opponentTeam Chess.Pawn


opponentBishop =
    Chess.Piece opponentTeam Chess.Bishop


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


knightMovesFromD4 =
    [ Position.b3, Position.c2, Position.e2, Position.f3, Position.f5, Position.e6, Position.c6, Position.b5 ]


position : Square -> ( Int, Int )
position (Chess.Occupied (Position px py) _) =
    ( px, py )


all : Test
all =
    describe "Chess"
        [ describe "check mate" <|
            [ describe "knows if check is counterable (block, take, escape)" <|
                [ test "no blocks, takes, or escape leads to checkmate" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.h1 monarch

                            attackers =
                                [ Chess.Occupied Position.g8 opponentRook
                                , Chess.Occupied Position.h7 opponentRook
                                ]

                            game =
                                Chess.init (attackers ++ [ current ]) team Nothing []
                        in
                        Expect.true "Double rooks should be checkmating here." (Chess.isCheckmate game)
                , test "if escapable then not checkmate" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.h1 monarch

                            attackers =
                                [ Chess.Occupied Position.h7 opponentRook
                                ]

                            game =
                                Chess.init (attackers ++ [ current ]) team Nothing []
                        in
                        Expect.false "Monarch may move out of check." (Chess.isCheckmate game)
                , test "if blockable then not checkmate" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.h1 monarch

                            attackers =
                                [ Chess.Occupied Position.g8 opponentRook
                                , Chess.Occupied Position.h7 opponentRook
                                ]

                            blocker =
                                Chess.Occupied Position.a2 rook

                            game =
                                Chess.init (attackers ++ [ current, blocker ]) team Nothing []
                        in
                        Expect.false "Rook should be able to block." (Chess.isCheckmate game)
                , test "if capturable then not checkmate" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.h1 monarch

                            attackers =
                                [ Chess.Occupied Position.g8 opponentRook
                                , Chess.Occupied Position.h3 opponentRook
                                ]

                            taker =
                                Chess.Occupied Position.f5 bishop

                            game =
                                Chess.init (attackers ++ [ current, taker ]) team Nothing []
                        in
                        Expect.false "Bishop should be able to capture the attacking Rook." (Chess.isCheckmate game)
                ]
            ]
        , describe "check" <|
            [ describe "knows if monarch is under fire" <|
                [ test "knows if any opposing piece may move to the monarchs square" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.b2 monarch

                            attackers =
                                [ Chess.Occupied Position.b1 opponentRook
                                , Chess.Occupied Position.c1 opponentBishop
                                , Chess.Occupied Position.a1 opponentAdvisor
                                ]

                            opponentThatCantAttack =
                                Chess.Occupied Position.h1 opponentRook

                            sameTeam =
                                Chess.Occupied Position.b3 rook

                            game =
                                Chess.init (attackers ++ [ current, opponentThatCantAttack, sameTeam ]) team Nothing []
                        in
                        Expect.equal (List.sortBy position attackers) (List.sortBy position (Chess.findChecks game))
                ]
            ]
        , describe "makeMove" <|
            [ test "removes the enpassant pawn when that is the move" <|
                \() ->
                    let
                        current =
                            Chess.Occupied Position.d4 pawn

                        pawnThatHoppedTwoCreatingEnpassant =
                            Chess.Occupied Position.e4 opponentPawn

                        game =
                            Chess.init [ current, pawnThatHoppedTwoCreatingEnpassant ] team (Just Position.e3) []

                        gameAfterEnpassantTake =
                            Chess.makeMove ( 4, 4 ) ( 5, 3 ) game
                    in
                    Expect.equal [ ( ( 5, 3 ), pawn ) ] (Dict.toList gameAfterEnpassantTake.occupiedSquares)
            , describe "castling moves" <|
                [ test "moves the Rook to the other side of the Monarch when castling - Black MonarchSide" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.e8 monarch

                            rookForCastle =
                                Chess.Occupied Position.h8 rook

                            game =
                                Chess.init [ current, rookForCastle ] team Nothing [ Chess.MonarchSide Chess.Black ]

                            gameAfterCastle =
                                Chess.makeMove (Position.toRaw Position.e8) (Position.toRaw Position.g8) game
                        in
                        Expect.equal
                            [ ( Position.toRaw Position.f8, rook )
                            , ( Position.toRaw Position.g8, monarch )
                            ]
                            (Dict.toList gameAfterCastle.occupiedSquares)
                , test "moves the Rook to the other side of the Monarch when castling - Black AdvisorSide" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.e8 monarch

                            rookForCastle =
                                Chess.Occupied Position.a8 rook

                            game =
                                Chess.init [ current, rookForCastle ] team Nothing [ Chess.AdvisorSide Chess.Black ]

                            gameAfterCastle =
                                Chess.makeMove (Position.toRaw Position.e8) (Position.toRaw Position.c8) game
                        in
                        Expect.equal
                            [ ( Position.toRaw Position.c8, monarch )
                            , ( Position.toRaw Position.d8, rook )
                            ]
                            (Dict.toList gameAfterCastle.occupiedSquares)
                , test "moves the Rook to the other side of the Monarch when castling - White MonarchSide" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.e1 opponentMonarch

                            rookForCastle =
                                Chess.Occupied Position.h1 opponentRook

                            game =
                                Chess.init [ current, rookForCastle ] opponentTeam Nothing [ Chess.MonarchSide Chess.White ]

                            gameAfterCastle =
                                Chess.makeMove (Position.toRaw Position.e1) (Position.toRaw Position.g1) game
                        in
                        Expect.equal
                            [ ( Position.toRaw Position.f1, opponentRook )
                            , ( Position.toRaw Position.g1, opponentMonarch )
                            ]
                            (Dict.toList gameAfterCastle.occupiedSquares)
                , test "moves the Rook to the other side of the Monarch when castling - White AdvisorSide" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.e1 opponentMonarch

                            rookForCastle =
                                Chess.Occupied Position.a1 opponentRook

                            game =
                                Chess.init [ current, rookForCastle ] opponentTeam Nothing [ Chess.AdvisorSide Chess.White ]

                            gameAfterCastle =
                                Chess.makeMove (Position.toRaw Position.e1) (Position.toRaw Position.c1) game
                        in
                        Expect.equal
                            [ ( Position.toRaw Position.c1, opponentMonarch )
                            , ( Position.toRaw Position.d1, opponentRook )
                            ]
                            (Dict.toList gameAfterCastle.occupiedSquares)
                , test "clears out castling rights if a move is made by the monarch" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.e1 monarch

                            game =
                                Chess.init [ current ] team Nothing [ Chess.AdvisorSide Chess.Black, Chess.AdvisorSide Chess.White ]

                            gameAfterCastle =
                                Chess.makeMove (Position.toRaw Position.e1) (Position.toRaw Position.e2) game
                        in
                        Expect.equal
                            [ Chess.AdvisorSide Chess.Black
                            ]
                            gameAfterCastle.castlingRights
                , test "clears out castling rights for that side only if a move is made by a rook" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.e8 monarch

                            rookForCastle =
                                Chess.Occupied Position.a8 rook

                            game =
                                Chess.init [ current, rookForCastle ] team Nothing [ Chess.AdvisorSide Chess.Black, Chess.MonarchSide Chess.Black ]

                            gameAfterCastle =
                                Chess.makeMove (Position.toRaw Position.a8) (Position.toRaw Position.a7) game
                        in
                        Expect.equal
                            [ Chess.AdvisorSide Chess.Black
                            ]
                            gameAfterCastle.castlingRights
                , test "clears out castling rights for that side only if a rook is captured" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.e8 monarch

                            rookForCastle =
                                Chess.Occupied Position.a8 rook

                            capturer =
                                Chess.Occupied Position.a6 opponentRook

                            game =
                                Chess.init [ current, rookForCastle, capturer ] opponentTeam Nothing [ Chess.AdvisorSide Chess.Black, Chess.MonarchSide Chess.Black ]

                            gameAfterCastle =
                                Chess.makeMove (Position.toRaw Position.a6) (Position.toRaw Position.a8) game
                        in
                        Expect.equal
                            [ Chess.AdvisorSide Chess.Black
                            ]
                            gameAfterCastle.castlingRights
                ]
            ]
        , describe "canMoveTo" <|
            [ describe "single piece" <|
                [ describe "monarch movement" <|
                    [ test "Valid" <|
                        \() ->
                            let
                                current =
                                    Chess.Occupied Position.b2 monarch

                                game =
                                    Chess.init [ current ] team Nothing []

                                validSquares =
                                    [ Position.a1
                                    , Position.a2
                                    , Position.a3
                                    , Position.b1
                                    , Position.b3
                                    , Position.c1
                                    , Position.c2
                                    , Position.c3
                                    ]
                            in
                            Expect.true
                                "Knows monarch moves"
                                (List.all
                                    (\pos -> Chess.canMoveTo pos game == [ Position.b2 ])
                                    validSquares
                                )
                    , test "knows castling rights" <|
                        \() ->
                            let
                                current =
                                    Chess.Occupied Position.e8 monarch

                                game =
                                    Chess.init [ current ] team Nothing [ Chess.AdvisorSide Chess.Black, Chess.MonarchSide Chess.Black ]

                                castlingMoves =
                                    [ Position.c8
                                    , Position.g8
                                    ]
                            in
                            Expect.equal castlingMoves
                                (List.filter
                                    (\pos -> Chess.canMoveTo pos game /= [])
                                    castlingMoves
                                )
                    , test "knows cannot castle if in check" <|
                        \() ->
                            let
                                current =
                                    Chess.Occupied Position.e8 monarch

                                checker =
                                    Chess.Occupied Position.e6 opponentRook

                                game =
                                    Chess.init [ current, checker ] team Nothing [ Chess.AdvisorSide Chess.Black, Chess.MonarchSide Chess.Black ]

                                castlingMoves =
                                    [ Position.c8
                                    , Position.g8
                                    ]
                            in
                            Expect.equal []
                                (List.filter
                                    (\pos -> Chess.canMoveTo pos game /= [])
                                    castlingMoves
                                )
                    , test "knows cannot castle if would move through check" <|
                        \() ->
                            let
                                current =
                                    Chess.Occupied Position.e8 monarch

                                checkerAdvisorSide =
                                    Chess.Occupied Position.f6 opponentRook

                                checkerMonarchSide =
                                    Chess.Occupied Position.c6 opponentRook

                                game =
                                    Chess.init [ current, checkerAdvisorSide, checkerMonarchSide ] team Nothing [ Chess.AdvisorSide Chess.Black, Chess.MonarchSide Chess.Black ]

                                castlingMoves =
                                    [ Position.c8
                                    , Position.g8
                                    ]
                            in
                            Expect.equal []
                                (List.filter
                                    (\pos -> Chess.canMoveTo pos game /= [])
                                    castlingMoves
                                )
                    , test "knows cannot castle if blocked by piece" <|
                        \() ->
                            let
                                current =
                                    Chess.Occupied Position.e8 monarch

                                blockedByFriendly =
                                    Chess.Occupied Position.f8 bishop

                                blockedByOpponent =
                                    Chess.Occupied Position.d8 opponentBishop

                                game =
                                    Chess.init [ current, blockedByFriendly, blockedByOpponent ] team Nothing [ Chess.AdvisorSide Chess.Black, Chess.MonarchSide Chess.Black ]

                                castlingMoves =
                                    [ Position.c8
                                    , Position.g8
                                    ]
                            in
                            Expect.equal []
                                (List.filter
                                    (\pos -> Chess.canMoveTo pos game /= [])
                                    castlingMoves
                                )
                    , test "invalid moves" <|
                        \() ->
                            let
                                current =
                                    Chess.Occupied Position.b2 monarch

                                game =
                                    Chess.init [ current ] team Nothing []

                                validSquares =
                                    [ Position.a1
                                    , Position.a2
                                    , Position.a3
                                    , Position.b1
                                    , Position.b3
                                    , Position.c1
                                    , Position.c2
                                    , Position.c3
                                    ]

                                otherSquares =
                                    List.filter (\s -> not (List.member s validSquares)) Position.all
                            in
                            Expect.true
                                "Not valid monarch movement"
                                (List.all
                                    (\pos -> Chess.canMoveTo pos game == [])
                                    otherSquares
                                )
                    ]
                , test "bishop movement" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.d4 bishop

                            game =
                                Chess.init [ current ] team Nothing []
                        in
                        Expect.true
                            "Knows bishop moves"
                            (List.all
                                (\pos -> Chess.canMoveTo pos game == [ Position.d4 ])
                                diagonalMovesFromD4
                            )
                , test "bishop invalid moves" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.d4 bishop

                            game =
                                Chess.init [ current ] team Nothing []

                            otherSquares =
                                List.filter (\s -> not (List.member s diagonalMovesFromD4)) Position.all
                        in
                        Expect.true
                            "Not valid bishop movement"
                            (List.all
                                (\pos -> Chess.canMoveTo pos game == [])
                                otherSquares
                            )
                , test "rook movement" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.d4 rook

                            game =
                                Chess.init [ current ] team Nothing []
                        in
                        Expect.true
                            "Knows rook moves"
                            (List.all
                                (\pos -> Chess.canMoveTo pos game == [ Position.d4 ])
                                horizontalMovesFromD4
                            )
                , test "Rook invalid moves" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.d4 rook

                            game =
                                Chess.init [ current ] team Nothing []

                            otherSquares =
                                List.filter (\s -> not (List.member s horizontalMovesFromD4)) Position.all
                        in
                        Expect.true
                            "Not valid rook movement"
                            (List.all
                                (\pos -> Chess.canMoveTo pos game == [])
                                otherSquares
                            )
                , test "Advisor movement" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.d4 advisor

                            game =
                                Chess.init [ current ] team Nothing []
                        in
                        Expect.true
                            "Knows advisor moves"
                            (List.all
                                (\pos -> Chess.canMoveTo pos game == [ Position.d4 ])
                                (horizontalMovesFromD4 ++ diagonalMovesFromD4)
                            )
                , test "Advisor invalid moves" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.d4 advisor

                            game =
                                Chess.init [ current ] team Nothing []

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
                                (\pos -> Chess.canMoveTo pos game /= [])
                                otherSquares
                            )
                , test "Knight movement" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.d4 knight

                            game =
                                Chess.init [ current ] team Nothing []
                        in
                        Expect.true
                            "Knows knight moves"
                            (List.all
                                (\pos -> Chess.canMoveTo pos game == [ Position.d4 ])
                                knightMovesFromD4
                            )
                , test "Knight in a corner" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.a1 knight

                            game =
                                Chess.init [ current ] team Nothing []
                        in
                        Expect.equal [ Position.b3, Position.c2 ]
                            (List.filter
                                (\pos -> Chess.canMoveTo pos game /= [])
                                Position.all
                            )
                , test "Knight invalid moves" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.d4 knight

                            game =
                                Chess.init [ current ] team Nothing []

                            otherSquares =
                                List.filter
                                    (\s ->
                                        not
                                            (List.member s
                                                knightMovesFromD4
                                            )
                                    )
                                    Position.all
                        in
                        Expect.equal []
                            (List.filter
                                (\pos -> Chess.canMoveTo pos game /= [])
                                otherSquares
                            )
                , describe "Pawn movement" <|
                    [ describe "Black" <|
                        [ test "From opening rank" <|
                            \() ->
                                let
                                    current =
                                        Chess.Occupied Position.b7 pawn

                                    enemyThatIsAttackable =
                                        Chess.Occupied Position.c6 opponentBishop

                                    friendly =
                                        Chess.Occupied Position.a6 pawn

                                    opponentBlockingFriendly =
                                        Chess.Occupied Position.a5 opponentPawn

                                    game =
                                        Chess.init [ current, enemyThatIsAttackable, friendly, opponentBlockingFriendly ] team Nothing []
                                in
                                Expect.equal [ Position.b6, Position.c6, Position.b5 ]
                                    (List.filter
                                        (\pos -> Chess.canMoveTo pos game /= [])
                                        Position.all
                                    )
                        , test "Not on opening rank" <|
                            \() ->
                                let
                                    current =
                                        Chess.Occupied Position.b6 pawn

                                    game =
                                        Chess.init [ current ] team Nothing []
                                in
                                Expect.equal [ Position.b5 ]
                                    (List.filter
                                        (\pos -> Chess.canMoveTo pos game /= [])
                                        Position.all
                                    )
                        , test "En passant is treated as a possible attack when considering." <|
                            \() ->
                                let
                                    current =
                                        Chess.Occupied Position.d4 pawn

                                    game =
                                        Chess.init [ current ] team (Just Position.e3) []
                                in
                                Expect.equal [ Position.d3, Position.e3 ]
                                    (List.filter
                                        (\pos -> Chess.canMoveTo pos game /= [])
                                        Position.all
                                    )
                        , test "En passant is ignored when not able to be reached." <|
                            \() ->
                                let
                                    current =
                                        Chess.Occupied Position.d5 pawn

                                    game =
                                        Chess.init [ current ] team (Just Position.e3) []
                                in
                                Expect.equal [ Position.d4 ]
                                    (List.filter
                                        (\pos -> Chess.canMoveTo pos game /= [])
                                        Position.all
                                    )
                        ]
                    , describe "White" <|
                        [ test "On opening rank" <|
                            \() ->
                                let
                                    current =
                                        Chess.Occupied Position.b2 opponentPawn

                                    enemyThatIsAttackable =
                                        Chess.Occupied Position.a3 bishop

                                    game =
                                        Chess.init [ current, enemyThatIsAttackable ] opponentTeam Nothing []
                                in
                                Expect.equal [ Position.b4, Position.a3, Position.b3 ]
                                    (List.filter
                                        (\pos -> Chess.canMoveTo pos game /= [])
                                        Position.all
                                    )
                        , test "Not on opening rank" <|
                            \() ->
                                let
                                    current =
                                        Chess.Occupied Position.b6 opponentPawn

                                    game =
                                        Chess.init [ current ] opponentTeam Nothing []
                                in
                                Expect.equal [ Position.b7 ]
                                    (List.filter
                                        (\pos -> Chess.canMoveTo pos game /= [])
                                        Position.all
                                    )
                        ]
                    ]
                ]
            , describe "more pieces" <|
                [ test "movement not allowed if blocked by pieces" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.a1 advisor

                            friendly pos =
                                Chess.Occupied pos rook

                            opponent pos =
                                Chess.Occupied pos opponentRook

                            game =
                                Chess.init [ friendly Position.a2, current, friendly Position.b1, opponent Position.b2 ] team Nothing []
                        in
                        Expect.equal [ Position.b2 ]
                            (List.filter
                                (\pos -> List.member Position.a1 (Chess.canMoveTo pos game))
                                Position.all
                            )
                , test "restrict movement by turn" <|
                    \() ->
                        let
                            current =
                                Chess.Occupied Position.a1 opponentAdvisor

                            game =
                                Chess.init [ current ] team Nothing []
                        in
                        Expect.equal [] (Chess.canMoveTo Position.a2 game)
                , test "moves may not result in check" <|
                    \() ->
                        let
                            protected =
                                Chess.Occupied Position.a1 monarch

                            pinned =
                                Chess.Occupied Position.b1 advisor

                            pinner =
                                Chess.Occupied Position.c1 opponentAdvisor

                            game =
                                Chess.init [ protected, pinned, pinner ] team Nothing []
                        in
                        Expect.equal [] (Chess.canMoveTo Position.b3 game)
                ]
            , describe "present recentMove" <|
                [ describe "when black" <|
                    [ test "south west" <|
                        \() ->
                            Chess.findVectors Position.h1 Position.g2 Chess.Black
                                |> Expect.equal ( 1, 1 )
                    , test "south east" <|
                        \() ->
                            Chess.findVectors Position.g1 Position.h2 Chess.Black
                                |> Expect.equal ( -1, 1 )
                    , test "north east" <|
                        \() ->
                            Chess.findVectors Position.h2 Position.g1 Chess.Black
                                |> Expect.equal ( 1, -1 )
                    , test "north west" <|
                        \() ->
                            Chess.findVectors Position.g2 Position.h1 Chess.Black
                                |> Expect.equal ( -1, -1 )
                    ]
                , describe "when white" <|
                    [ test "south west" <|
                        \() ->
                            Chess.findVectors Position.a8 Position.b7 Chess.White
                                |> Expect.equal ( 1, 1 )
                    , test "south east" <|
                        \() ->
                            Chess.findVectors Position.b8 Position.a7 Chess.White
                                |> Expect.equal ( -1, 1 )
                    , test "north east" <|
                        \() ->
                            Chess.findVectors Position.a7 Position.b8 Chess.White
                                |> Expect.equal ( 1, -1 )
                    , test "north west" <|
                        \() ->
                            Chess.findVectors Position.b7 Position.a8 Chess.White
                                |> Expect.equal ( -1, -1 )
                    ]
                ]
            ]
        ]
