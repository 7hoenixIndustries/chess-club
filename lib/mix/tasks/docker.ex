defmodule Mix.Tasks.Docker do
  use Mix.Task

  @app_name "chess_club"

  @shortdoc "Docker utilities for building releases."
  def run([env]) do
    build_image(env)

    {dir, _resp} = System.cmd("pwd", [])

    docker(
      "run -v #{String.trim(dir)}:/opt/build --rm -i #{@app_name}:latest /opt/build/bin/release #{env}"
    )
  end

  defp build_image(env) do
    secret_key_base = Application.fetch_env!(:chess_club, :secret_key_base)
    db_hostname = Application.fetch_env!(:chess_club, :db_hostname)
    db_username = Application.fetch_env!(:chess_club, :db_username)
    db_password = Application.fetch_env!(:chess_club, :db_password)

    args = [
      "env=#{env}",
      "db_port=5432",
      "db_hostname=#{db_hostname}",
      "db_username=#{db_username}",
      "db_password=#{db_password}",
      "secret_key_base=#{secret_key_base}",
    ]

    tmp = "build --build-arg #{Enum.join(args, " --build-arg ")} -t #{@app_name}:latest ."
    IO.inspect(tmp, label: "full command ==============\n")

    docker(tmp)
  end

  defp docker(cmd) do
    System.cmd("docker", String.split(cmd, " "), into: IO.stream(:stdio, :line))
  end
end