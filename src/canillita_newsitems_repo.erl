%%% @doc NewsItems repository
-module(canillita_newsitems_repo).

-export([ newspaper_exists/1, newsitem_exists/2 ]).

%% @doc Checks that there is a newspaper with the given name.
-spec newspaper_exists(NewspaperName::canillita_newspapers:name()) ->
  boolean().
newspaper_exists(NewspaperName) ->
  case sumo:find(canillita_newspapers, NewspaperName) of
    notfound -> false;
    _Entity -> true
  end.

%% @doc Returns the newsitem that matches the given conditions (if any).
-spec newsitem_exists(Model::module(), Condition::sumo:conditions()) ->
  notfound | sumo_rest_doc:entity().
newsitem_exists(Model, Conditions) ->
  case sumo:find_one(Model, Conditions) of
    notfound -> notfound;
    Entity -> Entity
  end.
