module Chess.Helpers exposing (advisor, bishop, monarch, opponentAdvisor, opponentBishop, opponentRook, opponentTeam, rook, team)

import Chess.Game as Chess exposing (Piece, PieceType(..), Square)


team =
    Chess.Black


opponentTeam =
    Chess.White


monarch =
    Chess.Piece team Chess.Monarch


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


opponentBishop =
    Chess.Piece opponentTeam Chess.Bishop
