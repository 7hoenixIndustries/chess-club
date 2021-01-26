module Chess.Position exposing (Position(..), a1, a2, a3, a4, a5, a6, a7, a8, all, b1, b2, b3, b4, b5, b6, b7, b8, c1, c2, c3, c4, c5, c6, c7, c8, d1, d2, d3, d4, d5, d6, d7, d8, e1, e2, e3, e4, e5, e6, e7, e8, f1, f2, f3, f4, f5, f6, f7, f8, g1, g2, g3, g4, g5, g6, g7, g8, h1, h2, h3, h4, h5, h6, h7, h8, toString)


type Position
    = Position Int Int


toString : Position -> String
toString (Position column row) =
    String.fromInt column ++ String.fromInt row


all : List Position
all =
    [ a8
    , b8
    , c8
    , d8
    , e8
    , f8
    , g8
    , h8
    , a7
    , b7
    , c7
    , d7
    , e7
    , f7
    , g7
    , h7
    , a6
    , b6
    , c6
    , d6
    , e6
    , f6
    , g6
    , h6
    , a5
    , b5
    , c5
    , d5
    , e5
    , f5
    , g5
    , h5
    , a4
    , b4
    , c4
    , d4
    , e4
    , f4
    , g4
    , h4
    , a3
    , b3
    , c3
    , d3
    , e3
    , f3
    , g3
    , h3
    , a2
    , b2
    , c2
    , d2
    , e2
    , f2
    , g2
    , h2
    , a1
    , b1
    , c1
    , d1
    , e1
    , f1
    , g1
    , h1
    ]


a8 : Position
a8 =
    Position 1 8


b8 : Position
b8 =
    Position 2 8


c8 : Position
c8 =
    Position 3 8


d8 : Position
d8 =
    Position 4 8


e8 : Position
e8 =
    Position 5 8


f8 : Position
f8 =
    Position 6 8


g8 : Position
g8 =
    Position 7 8


h8 : Position
h8 =
    Position 8 8


a7 : Position
a7 =
    Position 1 7


b7 : Position
b7 =
    Position 2 7


c7 : Position
c7 =
    Position 3 7


d7 : Position
d7 =
    Position 4 7


e7 : Position
e7 =
    Position 5 7


f7 : Position
f7 =
    Position 6 7


g7 : Position
g7 =
    Position 7 7


h7 : Position
h7 =
    Position 8 7


a6 : Position
a6 =
    Position 1 6


b6 : Position
b6 =
    Position 2 6


c6 : Position
c6 =
    Position 3 6


d6 : Position
d6 =
    Position 4 6


e6 : Position
e6 =
    Position 5 6


f6 : Position
f6 =
    Position 6 6


g6 : Position
g6 =
    Position 7 6


h6 : Position
h6 =
    Position 8 6


a5 : Position
a5 =
    Position 1 5


b5 : Position
b5 =
    Position 2 5


c5 : Position
c5 =
    Position 3 5


d5 : Position
d5 =
    Position 4 5


e5 : Position
e5 =
    Position 5 5


f5 : Position
f5 =
    Position 6 5


g5 : Position
g5 =
    Position 7 5


h5 : Position
h5 =
    Position 8 5


a4 : Position
a4 =
    Position 1 4


b4 : Position
b4 =
    Position 2 4


c4 : Position
c4 =
    Position 3 4


d4 : Position
d4 =
    Position 4 4


e4 : Position
e4 =
    Position 5 4


f4 : Position
f4 =
    Position 6 4


g4 : Position
g4 =
    Position 7 4


h4 : Position
h4 =
    Position 8 4


a3 : Position
a3 =
    Position 1 3


b3 : Position
b3 =
    Position 2 3


c3 : Position
c3 =
    Position 3 3


d3 : Position
d3 =
    Position 4 3


e3 : Position
e3 =
    Position 5 3


f3 : Position
f3 =
    Position 6 3


g3 : Position
g3 =
    Position 7 3


h3 : Position
h3 =
    Position 8 3


a2 : Position
a2 =
    Position 1 2


b2 : Position
b2 =
    Position 2 2


c2 : Position
c2 =
    Position 3 2


d2 : Position
d2 =
    Position 4 2


e2 : Position
e2 =
    Position 5 2


f2 : Position
f2 =
    Position 6 2


g2 : Position
g2 =
    Position 7 2


h2 : Position
h2 =
    Position 8 2


a1 : Position
a1 =
    Position 1 1


b1 : Position
b1 =
    Position 2 1


c1 : Position
c1 =
    Position 3 1


d1 : Position
d1 =
    Position 4 1


e1 : Position
e1 =
    Position 5 1


f1 : Position
f1 =
    Position 6 1


g1 : Position
g1 =
    Position 7 1


h1 : Position
h1 =
    Position 8 1
