defmodule ChessClubWeb.Router do
  use ChessClubWeb, :router

  import ChessClubWeb.UserAuth

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug :fetch_current_user
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  # NOTE: This :auth pipeline is for authenticating and does NOT mean that the underlying resource is authenticated.
  # For routes that are authenticated use the :ensure_auth pipeline instead.
  pipeline :auth do
    plug ChessClub.UserManager.Pipeline
  end

  pipeline :ensure_auth do
    plug Guardian.Plug.EnsureAuthenticated
  end

  pipeline :graphql do
    plug ChessClub.UserManager.Context
  end

  # LOGIN and LOGOUT
  scope "/", ChessClubWeb do
    pipe_through [:browser, :auth]

    get "/", PageController, :index

    get "/account", UserController, :new
    post "/account", UserController, :create

    get "/login", SessionController, :new
    post "/login", SessionController, :login
    get "/logout", SessionController, :logout
  end

  # Added for use while troubleshooting
  scope "/version", ChessClubWeb do
    get "/", VersionController, :index
  end

  scope "/", ChessClubWeb do
    pipe_through [:browser, :auth, :ensure_auth]

    get "/app", PageController, :app
  end

  scope "/api" do
    pipe_through [:api, :graphql]

    forward "/graphical", Absinthe.Plug.GraphiQL,
      schema: ChessClubWeb.Schema,
      interface: :playground,
      context: %{pubsub: ChessClubWeb.Endpoint}

    forward "/graphql", Absinthe.Plug, schema: ChessClubWeb.Schema
  end

  # Other scopes may use custom stacks.
  # scope "/api", ChessClubWeb do
  #   pipe_through :api
  # end

  # Enables LiveDashboard only for development
  #
  # If you want to use the LiveDashboard in production, you should put
  # it behind authentication and allow only admins to access it.
  # If your application does not have an admins-only section yet,
  # you can use Plug.BasicAuth to set up some basic authentication
  # as long as you are also using SSL (which you should anyway).
  if Mix.env() in [:dev, :test] do
    import Phoenix.LiveDashboard.Router

    scope "/" do
      pipe_through :browser
      live_dashboard "/dashboard", metrics: ChessClubWeb.Telemetry
    end
  end

  ## Authentication routes

  scope "/", ChessClubWeb do
    pipe_through [:browser, :redirect_if_user_is_authenticated]

    get "/users/register", UserRegistrationController, :new
    post "/users/register", UserRegistrationController, :create
    get "/users/log_in", UserSessionController, :new
    post "/users/log_in", UserSessionController, :create
    get "/users/reset_password", UserResetPasswordController, :new
    post "/users/reset_password", UserResetPasswordController, :create
    get "/users/reset_password/:token", UserResetPasswordController, :edit
    put "/users/reset_password/:token", UserResetPasswordController, :update
  end

  scope "/", ChessClubWeb do
    pipe_through [:browser, :require_authenticated_user]

    get "/users/settings", UserSettingsController, :edit
    put "/users/settings", UserSettingsController, :update
    get "/users/settings/confirm_email/:token", UserSettingsController, :confirm_email
  end

  scope "/", ChessClubWeb do
    pipe_through [:browser]

    delete "/users/log_out", UserSessionController, :delete
    get "/users/confirm", UserConfirmationController, :new
    post "/users/confirm", UserConfirmationController, :create
    get "/users/confirm/:token", UserConfirmationController, :confirm
  end
end
