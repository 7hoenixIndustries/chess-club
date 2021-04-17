use Mix.Config

# Only in tests, remove the complexity from the password hashing algorithm
config :argon2_elixir,
  t_cost: 1,
  m_cost: 8

# Configure your database
#
# The MIX_TEST_PARTITION environment variable can be used
# to provide built-in test partitioning in CI environment.
# Run `mix help test` for more information.
config :chess_club, ChessClub.Repo,
  username: "postgres",
  password: "postgres",
  database: "chess_club_test#{System.get_env("MIX_TEST_PARTITION")}",
  hostname: "localhost",
  pool: Ecto.Adapters.SQL.Sandbox,
  ssl: false

config :chess_club, ChessClub.Mailer, adapter: Bamboo.TestAdapter

# Run server in CI
config :chess_club, ChessClubWeb.Endpoint,
  http: [port: 4000],
  server: true

# Print only warnings and errors during test
config :logger, level: :warn
