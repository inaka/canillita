%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @author Fernando Benavides <elbrujohalcon@inaka.net>
%%% @doc Canillita Main Supervisor
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(canillita_sup).
-author('elbrujohalcon@inaka.net').

-behaviour(supervisor).

-export([start_link/0, start_listeners/0]).
-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ADMIN API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

start_listeners() ->
  case whereis(cowboy_sup) of
    undefined -> throw({missing, cowboy_sup});
    _Pid -> ok
  end,

  sumo:create_schema(),
  ok = pg2:create(canillita_listeners),
  
  Port = canillita:get_env(http_port, 4004),
  ListenerCount = canillita:get_env(http_listener_count, 20),

  Dispatch =
    cowboy_router:compile(
      [
        {'_',
          [
            {<<"/news">>, canillita_news_handler, []}
          ]
        }
      ]),

  RanchOptions =
    [ {port, Port}
    ],
  CowboyOptions =
    [ {env,       [{dispatch, Dispatch}]}
    , {compress,  true}
    , {timeout,   12000}
    ],
  
  cowboy:start_http(canillita_http, ListenerCount, RanchOptions, CowboyOptions).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init({}) ->
  {ok, { {one_for_one, 5, 10},
    [ {canillita_events,    {gen_event, start_link, [{local, canillita_events}]}, permanent, 5000, worker, [gen_event]}
    , {canillita_http,      {canillita_sup, start_listeners, []}, permanent, 1000, worker, [canillita_sup]}
    ]}
  }.
