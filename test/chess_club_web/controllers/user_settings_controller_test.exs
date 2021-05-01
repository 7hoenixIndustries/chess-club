defmodule ChessClubWeb.UserSettingsControllerTest do
  use ChessClubWeb.ConnCase, async: true

  import ChessClub.AccountsFixtures

  alias ChessClub.Accounts

  setup :register_and_log_in_user

  describe "GET /users/settings" do
    test "renders settings page", %{conn: conn} do
      edit_conn = get(conn, Routes.user_settings_path(conn, :edit))
      response = html_response(edit_conn, 200)
      assert response =~ "Settings"
    end

    test "redirects if user is not logged in" do
      conn = build_conn()
      edit_conn = get(conn, Routes.user_settings_path(conn, :edit))
      assert redirected_to(edit_conn) == Routes.user_session_path(edit_conn, :new)
    end
  end

  describe "PUT /users/settings (change password form)" do
    test "updates the user password and resets tokens", %{conn: conn, user: user} do
      new_password_conn =
        put(conn, Routes.user_settings_path(conn, :update), %{
          "action" => "update_password",
          "user" => %{
            "current_password" => valid_user_password(),
            "password" => "new valid password",
            "password_confirmation" => "new valid password"
          }
        })

      assert redirected_to(new_password_conn) == Routes.user_settings_path(conn, :edit)
      assert get_session(new_password_conn, :user_token) != get_session(conn, :user_token)
      assert get_flash(new_password_conn, :info) =~ "Password updated successfully"
      assert Accounts.get_user_by_email_and_password(user.email, "new valid password")
    end

    test "does not update password on invalid data", %{conn: conn} do
      old_password_conn =
        put(conn, Routes.user_settings_path(conn, :update), %{
          "action" => "update_password",
          "user" => %{
            "current_password" => "invalid",
            "password" => "too short",
            "password_confirmation" => "does not match"
          }
        })

      response = html_response(old_password_conn, 200)
      assert response =~ "Settings"
      assert response =~ "should be at least 12 character(s)"
      assert response =~ "does not match password"
      assert response =~ "is not valid"

      assert get_session(old_password_conn, :user_token) == get_session(conn, :user_token)
    end
  end

  describe "PUT /users/settings (change email form)" do
    @tag :capture_log
    test "updates the user email", %{conn: conn, user: user} do
      conn =
        put(conn, Routes.user_settings_path(conn, :update), %{
          "action" => "update_email",
          "user" => %{"current_password" => valid_user_password(), "email" => unique_user_email()}
        })

      assert redirected_to(conn) == Routes.user_settings_path(conn, :edit)
      assert get_flash(conn, :info) =~ "A link to confirm your email"
      assert Accounts.get_user_by_email(user.email)
    end

    test "does not update email on invalid data", %{conn: conn} do
      conn =
        put(conn, Routes.user_settings_path(conn, :update), %{
          "action" => "update_email",
          "user" => %{"current_password" => "invalid", "email" => "with spaces"}
        })

      response = html_response(conn, 200)
      assert response =~ "Settings"
      assert response =~ "must have the @ sign and no spaces"
      assert response =~ "is not valid"
    end
  end

  describe "GET /users/settings/confirm_email/:token" do
    setup %{user: user} do
      email = unique_user_email()

      token =
        extract_user_token(fn url ->
          Accounts.deliver_update_email_instructions(%{user | email: email}, user.email, url)
        end)

      %{token: token, email: email}
    end

    test "updates the user email once", %{conn: conn, user: user, token: token, email: email} do
      confirmed_conn = get(conn, Routes.user_settings_path(conn, :confirm_email, token))
      assert redirected_to(confirmed_conn) == Routes.user_settings_path(confirmed_conn, :edit)
      assert get_flash(confirmed_conn, :info) =~ "Email changed successfully"
      refute Accounts.get_user_by_email(user.email)
      assert Accounts.get_user_by_email(email)

      expired_conn =
        get(confirmed_conn, Routes.user_settings_path(confirmed_conn, :confirm_email, token))

      assert redirected_to(expired_conn) == Routes.user_settings_path(expired_conn, :edit)
      assert get_flash(expired_conn, :error) =~ "Email change link is invalid or it has expired"
    end

    test "does not update email with invalid token", %{conn: conn, user: user} do
      invalid_token_conn = get(conn, Routes.user_settings_path(conn, :confirm_email, "oops"))

      assert redirected_to(invalid_token_conn) ==
               Routes.user_settings_path(invalid_token_conn, :edit)

      assert get_flash(invalid_token_conn, :error) =~
               "Email change link is invalid or it has expired"

      assert Accounts.get_user_by_email(user.email)
    end

    test "redirects if user is not logged in", %{token: token} do
      conn = build_conn()
      confirmed_conn = get(conn, Routes.user_settings_path(conn, :confirm_email, token))
      assert redirected_to(confirmed_conn) == Routes.user_session_path(confirmed_conn, :new)
    end
  end
end
