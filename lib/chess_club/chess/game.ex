defmodule ChessClub.Chess.Game do
  @moduledoc "Encapsulates a python process to get available moves"
  use GenServer

  alias ChessClub.Chess.Move
  @expected_fields ~w(
                      moves current_state
                    )

  def start_link(opts) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  ## CLIENT

  @doc """
  Will call out to the underlying Python server over Erlport to get available chess moves.
  """
  def available_moves(server, fen, moves_made) do
    GenServer.call(server, {:available_moves, %{fen: fen, moves_made: moves_made}})
  end

  def legal_move(server, fen, moves_made, move) do
    GenServer.call(server, {:legal_move, %{fen: fen, moves_made: moves_made, move: move}})
  end

  def moves_played(server, fen, moves_made) do
    GenServer.call(server, {:moves_played, %{fen: fen, moves_made: moves_made}})
  end

  ## SERVER

  @impl GenServer
  def init(:ok) do
    {:ok, py} = :python.start([{:python_path, './api'}, {:python, 'python3'}])
    {:ok, %{chess_server: py}}
  end

  @impl GenServer
  def handle_call({:available_moves, %{fen: fen, moves_made: moves_made}}, _from, %{
        chess_server: server
      }) do
    {:ok, request_body} = Poison.encode(%{board: fen, moves_made: moves_made})
    body = :python.call(server, :app, :route_moves_erlport, [request_body])

    %{moves: moves, current_state: current_state} = extract_response(body)

    {:reply, %{moves: Enum.map(moves, &to_move/1), current_state: current_state},
     %{chess_server: server}}
  end

  def handle_call({:moves_played, %{fen: fen, moves_made: moves_made}}, _from, %{
        chess_server: server
      }) do
    {:ok, request_body} = Poison.encode(%{board: fen, moves_made: moves_made})

    %{"moves_played" => moves_played} =
      Poison.decode!(:python.call(server, :app, :moves_played, [request_body]))

    played = Enum.map(moves_played, &to_previous_move/1)
    {:reply, %{moves_played: played}, %{chess_server: server}}
  end

  def handle_call({:legal_move, %{fen: fen, moves_made: moves_made, move: move}}, _from, %{
        chess_server: server
      }) do
    {:ok, request_body} = Poison.encode(%{board: fen, moves_made: moves_made, move: move})

    %{"is_legal" => is_legal} =
      Poison.decode!(:python.call(server, :app, :legal_move, [request_body]))

    {:reply, %{is_legal: is_legal}, %{chess_server: server}}
  end

  defp extract_response(body) do
    body
    |> Poison.decode!()
    |> Map.take(@expected_fields)
    |> Enum.reduce(%{}, fn {key, val}, acc -> Map.put(acc, String.to_existing_atom(key), val) end)
  end

  defp to_move(%{
         "from" => square_from,
         "to" => square_to,
         "player" => color,
         "command" => move_command,
         "fenAfterMove" => fen_after_move
       }) do
    c =
      case color do
        "WHITE" -> "w"
        "BLACK" -> "b"
      end

    %Move{
      square_from: square_from,
      square_to: square_to,
      color: c,
      move_command: move_command,
      fen_after_move: fen_after_move
    }
  end

  defp to_previous_move(%{
         "fen_after_move" => fen_after_move,
         "previous_move" => previous_move
       }) do
    %{
      fen_after_move: fen_after_move,
      previous_move: previous_move
    }
  end
end
