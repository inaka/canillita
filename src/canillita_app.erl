-module(canillita_app).
-behaviour(application).

-export([start/0, start/2, start_phase/3]).
-export([stop/0, stop/1]).

%% @doc Starts the Application
-spec start() -> {ok, [atom()]} | {error, term()}.
start() -> {ok, _} = application:ensure_all_started(canillita).

%% @doc Stops the Application
-spec stop() -> ok | {error, term()}.
stop() -> ok = application:stop(canillita).

-spec start(Type::application:start_type(), Args::any()) -> {ok, pid()}.
start(_Type, _Args) -> {ok, self()}.

-spec stop(State::[]) -> ok.
stop(_State) -> ok.

%% @private
-spec start_phase(atom(), StartType::application:start_type(), []) ->
  ok | {error, _}.
start_phase(create_schema, _StartType, []) ->
  _ = application:stop(mnesia),
  Node = node(),
  case mnesia:create_schema([Node]) of
    ok -> ok;
    {error, {Node, {already_exists, Node}}} -> ok
  end,
  {ok, _} = application:ensure_all_started(mnesia),
  sumo:create_schema().
