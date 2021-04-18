defmodule ChessClubWeb.PageController do
  use ChessClubWeb, :controller

  alias ChessClub.Accounts.UserToken

  def index(conn, _params) do
    conn
    # This is the default layout. Just being explicit.
    |> put_layout("index.html")
    |> render("index.html", maybe_current_user: conn.assigns.current_user)
  end

  def app(conn, _) do
    user = conn.assigns[:current_user]
    token = Base.encode64(get_session(conn, :user_token))

    conn
    |> put_layout("app.html")
    |> render("app.html", current_user: user, auth_token: token)
  end
end
