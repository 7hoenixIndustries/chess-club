defmodule ChessClub.Email.WelcomeTest do
  use ExUnit.Case
  alias ChessClub.Email.Welcome

  test "welcome email" do
    email_address = "person@example.com"

    email = Welcome.welcome_email(email_address)

    assert email.to == email_address
    assert email.subject == "Welcome to the club"
    assert email.html_body =~ "Sign up"
  end
end
