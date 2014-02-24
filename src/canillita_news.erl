%% @doc News model
-module(canillita_news).
-author('elbrujohalcon@inaka.net').

-behaviour(sumo_doc).

-record(canillita_new,
  { id          :: pos_integer()
  , title       :: binary()
  , content     :: binary()
  , created_at  :: canillita_utils:datetime()
  , updated_at  :: canillita_utils:datetime()
  }).


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

-spec sumo_sleep(#canillita_new{}) -> proplists:proplists().
sumo_sleep(NewsFlash) ->
  [ {id,          NewsFlash#canillita_new.id}
  , {title,       NewsFlash#canillita_new.title}
  , {content,     NewsFlash#canillita_new.content}
  , {created_at,  NewsFlash#canillita_new.created_at}
  , {updated_at,  NewsFlash#canillita_new.updated_at}
  ].

-spec sumo_wakeup(proplists:proplists()) -> #canillita_new{}.
sumo_wakeup(NewsFlash) -> 
  #canillita_new{
    id         = proplists:get_value(id, NewsFlash),
    title      = proplists:get_value(title, NewsFlash),
    content    = proplists:get_value(content, NewsFlash),
    created_at = proplists:get_value(created_at, NewsFlash),
    updated_at = proplists:get_value(updated_at, NewsFlash)
  }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates, stores and returns a news flash.
new(Title, Content) ->
  Now = {datetime, calendar:universal_time()},
  NewsFlash = #canillita_new{
    title      =  Title,
    content    =  Content,
    created_at =  Now,
    updated_at =  Now
  },
  sumo:persist(canillita_news, NewsFlash).

get_id(NewsFlash) -> NewsFlash#canillita_new.id.

get_title(NewsFlash) -> NewsFlash#canillita_new.title.

get_content(NewsFlash) -> NewsFlash#canillita_new.content.

latest_news() -> lists:reverse(sumo:find_all(canillita_news)).
