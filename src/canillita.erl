-module(canillita).
-behaviour(application).

-export([start/0, start/2, start_phase/3]).
-export([stop/0, stop/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ADMIN API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Starts the Application
-spec start() -> {ok, [atom()]} | {error, term()}.
start() -> {ok, _} = application:ensure_all_started(canillita).

%% @doc Stops the Application
-spec stop() -> ok | {error, term()}.
stop() -> ok = application:stop(canillita).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec start(Type::application:start_type(), Args::any()) -> {ok, pid()}.
start(_Type, _Args) -> {ok, self()}.

-spec stop(State::[]) -> ok.
stop(_State) ->
  gen_event:delete_handler( canillita_newsitems_events_manager
                          , canillita_newsitems_events_handler
                          , []
                          ),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% START PHASES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
  % Create persistency schema
  sumo:create_schema();
start_phase(start_cowboy_listeners, _StartType, []) ->
  Handlers =
    [ canillita_newspapers_handler
    , canillita_single_newspaper_handler
    , canillita_newsitems_handler
    , canillita_single_newsitem_handler
    , canillita_news_handler
    , cowboy_swagger_handler
    ],
  % Get the trails for each handler
  Routes = trails:trails(Handlers),
  % Store them so Cowboy is able to get them
  trails:store(Routes),
  % Set server routes
  Dispatch = trails:single_host_compile(Routes),
  % Set the options for the TCP layer
  TransOpts = [{port, 4892}],
  % Set the options for the HTTP layer
  ProtoOpts = [{env, [{dispatch, Dispatch}, {compress, true}]}],
  % Start Cowboy HTTP server
  case cowboy:start_http(canillita_server, 1, TransOpts, ProtoOpts) of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok
  end;
start_phase(start_canillita_events_management, _StartType, []) ->
  % Set the handler for processing SumoDB events
  ok = gen_event:add_handler( canillita_newsitems_events_manager
                            , canillita_newsitems_events_handler
                            , []
                            ),
  % Create pg2 group to hold news listeners
  pg2:create(canillita_listeners).
