defmodule ChessClubWeb.UserSocket do
  use Phoenix.Socket
  use Absinthe.Phoenix.Socket, schema: ChessClubWeb.Schema

  alias ChessClub.Accounts

  require Logger

  ## Channels
  # channel "room:*", ChessClubWeb.RoomChannel

  # Socket params are passed from the client and can
  # be used to verify and authenticate a user. After
  # verification, you can put default assigns into
  # the socket that will be set for all channels, ie
  #
  #     {:ok, assign(socket, :user_id, verified_user_id)}
  #
  # To deny connection, return `:error`.
  #
  # See `Phoenix.Token` documentation for examples in
  # performing token verification on connect.
  @impl Phoenix.Socket
  def connect(%{"auth_token" => encoded_token}, socket, _connect_info) do
    with {:ok, user_token} <- Base.decode64(encoded_token),
         %ChessClub.Accounts.User{} = user <- Accounts.get_user_by_session_token(user_token) do
      authenticated_socket =
        Absinthe.Phoenix.Socket.put_options(socket,
          context: %{
            current_user: user
          }
        )

      {:ok, authenticated_socket}
    else
      result ->
        Logger.info("Not authenticaed in socket connection: #{result}.")
        {:ok, socket}
    end
  end

  def connect(_params, _socket, _connect_info) do
    :error
  end

  # Socket id's are topics that allow you to identify all sockets for a given user:
  #
  #     def id(socket), do: "user_socket:#{socket.assigns.user_id}"
  #
  # Would allow you to broadcast a "disconnect" event and terminate
  # all active sockets and channels for a given user:
  #
  #     ChessClubWeb.Endpoint.broadcast("user_socket:#{user.id}", "disconnect", %{})
  #
  # Returning `nil` makes this socket anonymous.
  @impl Phoenix.Socket
  def id(_socket), do: nil
end
