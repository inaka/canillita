-module(canillita_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  NewsItemsEventManager =
    #{ id =>  canillita_newsitems_events_manager
     , start => { gen_event
                , start_link
                , [{local, canillita_newsitems_events_manager}]
                }
     },
  Procs = [NewsItemsEventManager],
  {ok, {{one_for_one, 1, 5}, Procs}}.
