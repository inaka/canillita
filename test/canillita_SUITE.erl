-module(canillita_SUITE).

-export([all/0]).
-export([canillita_start_test/1, canillita_stop_test/1]).

-type config() :: proplists:proplist().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common Test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec all() -> [atom()].
all() -> [canillita_start_test, canillita_stop_test].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec canillita_start_test(Config::config()) ->
  config().
canillita_start_test(Config) ->
  {ok, _} = application:ensure_all_started(canillita),
  Config.

-spec canillita_stop_test(Config::config()) ->
  config().
canillita_stop_test(Config) ->
  ok = application:stop(canillita),
  Config.
