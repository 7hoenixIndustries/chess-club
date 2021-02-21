defmodule ChessClub.Learn.Move do
  @moduledoc "Ecto Schema for moves in a scenario"

  use Ecto.Schema
  import Ecto.Changeset
  alias ChessClub.Learn.Scenario
  # Feels like we are mixing concerns.
  alias ChessClub.Chess.Game
  alias ChessClub.Repo

  schema "moves" do
    field :move_command, :string
    belongs_to :scenario, Scenario

    timestamps()
  end

  @doc false
  def changeset(move, attrs) do
    move
    |> cast(attrs, [:move_command, :scenario_id])
    |> validate_required([:move_command, :scenario_id])
    |> validate_legal([:move_command, :scenario_id])
  end

  def validate_legal(changeset, _) do
    case changeset.changes do
      %{move_command: move_command, scenario_id: scenario_id} ->
        Repo.get(Scenario, scenario_id)
        |> Repo.preload([:moves])
        |> validate_legal_inner(changeset, move_command)
    end
  end

  def validate_legal_inner(scenario, changeset, move_command) do
    validate_change(changeset, :move_command, fn _, _ ->
      scenario_moves = Enum.map(scenario.moves, fn m -> m.move_command end)

      %{is_legal: is_legal} =
        Game.legal_move(Game, scenario.starting_state, scenario_moves, move_command)

      if is_legal do
        []
      else
        [move_command: "makeMove failed. . . out of sync."]
      end
    end)
  end
end
