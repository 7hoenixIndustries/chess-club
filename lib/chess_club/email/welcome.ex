defmodule ChessClub.Email.Welcome do
  @moduledoc """
  Welcome email. Currently just a spike and not used.
  """
  import Bamboo.Email

  def welcome_email(to_email) do
    # _random_number = :rand.uniform() |> (fn r -> r * 1000 end).() |> floor

    new_email(
      # to: "#{to_email}+#{random_number}@#{to_domain}.com",
      to: "#{to_email}",
      from: "alerts@7hoenix.com",
      subject: "Welcome to the club",
      html_body: "<strong>Sign up</strong>",
      text_body: "Sign up"
    )
  end
end
