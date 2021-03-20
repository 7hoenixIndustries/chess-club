defmodule ChessClub.Email.Welcome do
  import Bamboo.Email

  def welcome_email(to_email, to_domain) do
    random_number = :rand.uniform() |> (fn r -> r * 1000 end).() |> floor

    new_email(
      to: "#{to_email}+#{random_number}@#{to_domain}.com",
      from: "alerts@club.7hoenix.com",
      subject: "Welcome to club",
      html_body: "<strong>Sign up</strong>",
      text_body: "Sign up"
    )
  end
end
