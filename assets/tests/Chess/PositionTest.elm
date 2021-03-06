module Chess.PositionTest exposing (all)

import Chess.Position as Position exposing (Position(..))
import Expect
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Position"
        [ describe "movement vectors" <|
            [ describe "compares for black" <|
                [ test "south west" <|
                    \() ->
                        Position.compare Position.h1 Position.g2
                            |> Expect.equal ( -1, 1 )
                ]
            ]
        ]
