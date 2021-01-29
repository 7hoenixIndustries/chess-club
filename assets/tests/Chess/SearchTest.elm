module Chess.SearchTest exposing (all)

import Chess.Board as Board exposing (Position(..))
import Chess.Game as Game
import Chess.Helpers exposing (..)
import Chess.Search as Search
import Expect exposing (..)
import Test exposing (..)


position : Board.Position -> ( Int, Int )
position (Board.Position px py) =
    ( px, py )


all : Test
all =
    describe "Chess"
        [ describe "forcingMoves" <|
            [ describe "recognizes checkmate as bottom" <|
                [ test "will find checkmate" <|
                    \() ->
                        let
                            current =
                                Game.Occupied (position Board.h1) monarch

                            attackers =
                                [ Game.Occupied (position Board.g8) opponentRook
                                , Game.Occupied (position Board.g7) opponentRook
                                ]

                            game =
                                Search.init (Game.initWithExistingState (attackers ++ [ current ]) (Game.Turn opponentTeam))
                        in
                        Expect.true "its not there!" (List.member "77-87 CheckMate > Root" (Search.forcingMoves game))
                , test "will find checkmate with a check in between" <|
                    \() ->
                        let
                            current =
                                Game.Occupied (position Board.g1) monarch

                            attackers =
                                [ Game.Occupied (position Board.e8) opponentRook
                                , Game.Occupied (position Board.f7) opponentRook
                                ]

                            game =
                                Search.init (Game.initWithExistingState (attackers ++ [ current ]) (Game.Turn opponentTeam))
                        in
                        Expect.true "its not there!" (List.member "67-87 CheckMate > 71-81 Forced > 58-78 Check" (Search.forcingMoves game))

                --[ test "will lose material to achieve checkmate" <|
                --describe "recognizes material as a kind of forcing move" <|
                -- [test "but still prefers checkmate given the option" <|
                ]
            ]
        ]
