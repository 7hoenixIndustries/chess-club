defmodule ChessClubWeb.ConnCase do
  @moduledoc """
  This module defines the test case to be used by
  tests that require setting up a connection.

  Such tests rely on `Phoenix.ConnTest` and also
  import other functionality to make it easier
  to build common data structures and query the data layer.

  Finally, if the test case interacts with the database,
  we enable the SQL sandbox, so changes done to the database
  are reverted at the end of every test. If you are using
  PostgreSQL, you can even run database tests asynchronously
  by setting `use ChessClubWeb.ConnCase, async: true`, although
  this option is not recommended for other databases.
  """

  use ExUnit.CaseTemplate

  alias ChessClub.Factory
  alias Ecto.Adapters.SQL.Sandbox
  alias ChessClub.AccountsFixtures

  using do
    quote do
      # Import conveniences for testing with connections
      import Plug.Conn
      import Phoenix.ConnTest
      import ChessClubWeb.ConnCase

      alias ChessClub.Factory
      # credo:disable-for-next-line
      alias ChessClubWeb.Router.Helpers, as: Routes

      # The default endpoint for testing
      @endpoint ChessClubWeb.Endpoint
    end
  end

  setup tags do
    :ok = Sandbox.checkout(ChessClub.Repo)

    unless tags[:async] do
      Sandbox.mode(ChessClub.Repo, {:shared, self()})
    end

    {:ok, conn: Phoenix.ConnTest.build_conn()}
  end

  @doc """
  Setup helper that registers and logs in users.

      setup :register_and_log_in_user

  It stores an updated connection and a registered user in the
  test context.
  """
  def register_and_log_in_user(%{conn: conn}) do
    user = ChessClub.AccountsFixtures.user_fixture()
    %{conn: log_in_user(conn, user), user: user}
  end

  @doc """
  Logs the given `user` into the `conn`.

  It returns an updated `conn`.
  """
  def log_in_user(conn, user) do
    token = ChessClub.Accounts.generate_user_session_token(user)

    conn
    |> Phoenix.ConnTest.init_test_session(%{})
    |> Plug.Conn.put_session(:user_token, token)
  end

  def register_and_authenticate_user_for_api(%{conn: conn}) do
    user = AccountsFixtures.user_fixture()
    token = ChessClub.Accounts.generate_user_session_token(user)

    authorized_conn =
      Plug.Conn.put_req_header(
        Phoenix.ConnTest.build_conn(),
        "authorization",
        "Bearer #{Base.encode64(token)}"
      )

    %{conn: conn, user: user, authorized_conn: authorized_conn}
  end
end
