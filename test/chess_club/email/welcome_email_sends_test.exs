defmodule ChessClub.Email.WelcomeEmailSendsTest do
  use ExUnit.Case
  use Bamboo.Test
  alias ChessClub.Email.Welcome

  test "sends welcome email" do
    email = Welcome.welcome_email("some@email.com")

    ChessClub.Mailer.deliver_now(email)

    # Works with deliver_now and deliver_later
    assert_delivered_email(email)
  end
end
