%%% @doc Newspapers repository
-module(canillita_newspapers_repo).

-export([exists/1]).

%% @doc Checks that there is a newspaper with the given name.
-spec exists(NewspaperName::canillita_newspapers:name()) -> boolean().
exists(NewspaperName) ->
  notfound /= sumo:fetch(canillita_newspapers, NewspaperName).
