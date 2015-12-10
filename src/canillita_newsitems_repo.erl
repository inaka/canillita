%%% @doc NewsItems repository
-module(canillita_newsitems_repo).

-export([fetch/2]).

%% @doc Returns the newsitem that matches the given
%% newspaper_name and id (if any).
-spec fetch( NewspaperName::canillita_newspapers:name()
           , Id::canillita_newsitems:id()
           ) -> notfound | sumo_rest_doc:entity().
fetch(NewspaperName, Id) ->
  Conditions = [{id, Id}, {newspaper_name, NewspaperName}],
  sumo:find_one(canillita_newsitems, Conditions).
