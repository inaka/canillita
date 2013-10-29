%% @doc News model
-module(canillita_news).
-author('elbrujohalcon@inaka.net').

-behaviour(sumo_doc).

%%% Public API
-export(
  [ new/2
  , get_id/1
  , get_title/1
  , get_content/1
  , latest_news/0
  ]).
%%% Behaviour callbacks.
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sumo_schema() ->
  sumo:new_schema(?MODULE,
    [ sumo:new_field(id,            integer,  [id, not_null, auto_increment])
    , sumo:new_field(title,         text,     [not_null])
    , sumo:new_field(content,       text,     [not_null])
    , sumo:new_field(created_at,    datetime, [not_null])
    , sumo:new_field(updated_at,    datetime, [not_null])
    ]).

sumo_sleep(NewsFlash) -> NewsFlash.

sumo_wakeup(NewsFlash) -> NewsFlash.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates, stores and returns a news flash.
new(Title, Content) ->
  Now = {datetime, calendar:universal_time()},
  NewsFlash =
    [ {title,       Title}
    , {content,     Content}
    , {created_at,  Now}
    , {updated_at,  Now}],
  sumo:persist(canillita_news, NewsFlash).

get_id(NewsFlash) -> proplists:get_value(id, NewsFlash).

get_title(NewsFlash) -> proplists:get_value(title, NewsFlash).

get_content(NewsFlash) -> proplists:get_value(content, NewsFlash).

latest_news() -> sumo:find_all(canillita_news).
