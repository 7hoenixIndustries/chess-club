defmodule ChessClubWeb.PageControllerTest do
  use ChessClubWeb.ConnCase

  test "GET /", %{conn: conn} do
    conn = get(conn, "/")
    assert html_response(conn, 200) =~ "Online Chess Club"
  end

  describe "GET /app" do
    test "redirects not logged in users", %{conn: conn} do
      conn = get(conn, "/app")
      assert redirected_to(conn) =~ Routes.user_session_path(conn, :new)
    end

    # test "passes the token down to Elm", %{conn: conn} do
    #   conn = get(conn, "/app")
    #   assert redirected_to(conn) =~ Routes.user_session_path(conn, :new)
    # end
  end
end
