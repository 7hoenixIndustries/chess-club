import chess
import json


def route_moves_erlport(event):
    as_json = json.loads(event)
    return json.dumps(moves(as_json["board"], as_json["moves_made"]))

def legal_move(event):
    as_json = json.loads(event)
    return json.dumps(is_legal(as_json["board"], as_json["moves_made"], as_json["move"]))

def moves_played(event):
    as_json = json.loads(event)
    return json.dumps(moves_already_played(as_json["board"], as_json["moves_made"]))

# MOVES

def is_legal(board, moves_made, move):
    chess_board = chess.Board(board)
    for m in moves_made:
        chess_board.push(chess.Move.from_uci(m))
    return {
        "is_legal": chess.Move.from_uci(move) in chess_board.legal_moves
    }

def moves(board, moves_made):
    chess_board = chess.Board(board)
    for move in moves_made:
        chess_board.push(chess.Move.from_uci(move))
    assert chess_board.is_valid()
    return {
        "current_state": chess_board.fen(),
        "moves": possible_moves(chess_board.copy()),
        "turn": turn(chess_board),
    }

def moves_already_played(board, moves_made):
    chess_board = chess.Board(board)
    mp = []
    for move in moves_made:
        move_as_uci = chess.Move.from_uci(move)
        chess_board.push(move_as_uci)
        mp.append({"fen_after_move": chess_board.fen(), "previous_move": move_as_uci.uci()})
    return {"moves_played": mp}

def possible_moves(chess_board):
    first_moves = possible_moves_for(chess_board.copy())
    chess_board.push(chess.Move.null())
    second_moves = possible_moves_for(chess_board.copy())
    chess_board.pop()
    return list(first_moves) + list(second_moves)

def possible_moves_for(chess_board):
    return map(
        lambda chess_move: {
            "from": chess.SQUARE_NAMES[chess_move.from_square],
            "to": chess.SQUARE_NAMES[chess_move.to_square],
            "command": chess_move.uci(),
            "promotion": piece_name(chess_move.promotion),
            "player": turn(chess_board),
            "fenAfterMove": try_move(chess_move, chess_board),
        },
        chess_board.legal_moves
    )

def piece_name(chess_id):
    names = {
        None: None,
        chess.PAWN: "PAWN",
        chess.KNIGHT: "KNIGHT",
        chess.BISHOP: "BISHOP",
        chess.ROOK: "ROOK",
        chess.QUEEN: "ADVISOR",
        chess.KING: "MONARCH",
    }
    return names[chess_id]

def turn(chess_board):
    return "WHITE" if chess_board.turn else "BLACK"

def try_move(move, board):
    if move in board.legal_moves:
        board.push(move)
        fen = str(board.fen())
        board.pop()
        return fen
    else:
        fen = str(board.fen())
        return fen

