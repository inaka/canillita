%%% @doc NewsFlash Model
-module(canillita_newsflash).

-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).

-type id() :: binary().
-type newspaper_name() :: binary().
-type title() :: binary().
-type body() :: binary().

-opaque news_flash() ::
  #{ id => id() | undefined
   , newspaper_name => newspaper_name()
   , title => title()
   , body => body()
   , created_at => calendar:datetime()
   , updated_at => calendar:datetime()
   }.

-export_type(
  [ id/0
  , newspaper_name/0
  , title/0
  , body/0
  , news_flash/0]
  ).

%% sumo_doc behaviour callbacks
-export(
  [ sumo_schema/0
  , sumo_sleep/1
  , sumo_wakeup/1
  ]).

%% sumo_rest_doc behaviour callbacks
-export(
  [ to_json/1
  , from_json/1
  , from_json/2
  , update/2
  , uri_path/1
  ]).

%% Public API
-export(
  [ new/3
  , newspaper/1
  , title/1
  , body/1
  , updated_at/1
  ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_doc behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(
    ?MODULE,
    [ sumo:new_field(id, binary, [id, unique])
    , sumo:new_field(newspaper_name, string, [not_null])
    , sumo:new_field(title, string, [not_null])
    , sumo:new_field(body, binary, [not_null])
    , sumo:new_field(created_at, datetime, [not_null])
    , sumo:new_field(updated_at, datetime, [not_null])
    ]).

-spec sumo_sleep(NewsFlash::news_flash()) -> sumo:doc().
sumo_sleep(NewsFlash) -> NewsFlash.

-spec sumo_wakeup(NewsFlash::sumo:doc()) -> news_flash().
sumo_wakeup(NewsFlash) -> NewsFlash.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_rest_doc behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec to_json(NewsFlash::news_flash()) -> news_flash().
to_json(NewsFlash) ->
  #{ id => sr_json:encode_null(maps:get(id, NewsFlash))
   , newspaper_name => maps:get(newspaper_name, NewsFlash)
   , title => maps:get(title, NewsFlash)
   , body => maps:get(body, NewsFlash)
   , created_at => sr_json:encode_date(maps:get(created_at, NewsFlash))
   , updated_at => sr_json:encode_date(maps:get(updated_at, NewsFlash))
   }.

-spec from_json(NewspaperName::newspaper_name(), Json::sumo_rest_doc:json()) ->
  {ok, news_flash()} | {error, iodata()}.
from_json(NewspaperName, Json) ->
  from_json(Json#{<<"newspaper_name">> => NewspaperName}).

-spec from_json(Json::sumo_rest_doc:json()) ->
  {ok, news_flash()} | {error, iodata()}.
from_json(Json) ->
  io:format("Json: ~p~n", [Json]),
  Now = sr_json:encode_date(calendar:universal_time()),
  try
    { ok
    , #{ id => sr_json:decode_null(maps:get(<<"id">>, Json, null))
       , newspaper_name => maps:get(<<"newspaper_name">>, Json)
       , title => maps:get(<<"title">>, Json)
       , body => maps:get(<<"body">>, Json)
       , created_at =>
          sr_json:decode_date(maps:get(<<"created_at">>, Json, Now))
       , updated_at =>
          sr_json:decode_date(maps:get(<<"updated_at">>, Json, Now))
       }
    }
  catch
    _:{badkey, Key} -> {error, <<"missing field: ", Key/binary>>}
  end.

-spec update(NewsFlash::news_flash(), Json::sumo_rest_doc:json()) ->
  {ok, news_flash()} | {error, iodata()}.
update(NewsFlash, Json) ->
  try
    MergedNews = maps:merge(NewsFlash, Json),
    UpdatedNews = MergedNews#{updated_at := calendar:universal_time()},
    {ok, UpdatedNews}
  catch
      _:{badkey, Key} -> {error, <<"missing field: ", Key/binary>>}
  end.

%% @doc Specify the URI part that uniquely identifies a NewsFlash.
-spec uri_path(NewsFlash::news_flash()) -> id().
uri_path(#{id := NewsId}) -> NewsId.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(NewspaperName::newspaper_name(), Title::title(), Body::body()) ->
  news_flash().
new(NewspaperName, Title, Body) ->
  Now = calendar:universal_time(),
  #{ id => undefined
   , newspaper_name => NewspaperName
   , title => Title
   , body => Body
   , created_at => Now
   , updated_at => Now
   }.

-spec newspaper(NewsFlash::news_flash()) -> newspaper_name().
newspaper(#{newspaper_name := Newspaper}) -> Newspaper.

-spec title(NewsFlash::news_flash()) -> title().
title(#{title := Title}) -> Title.

-spec body(NewsFlash::news_flash()) -> body().
body(#{body := Body}) -> Body.

-spec updated_at(NewsFlash::news_flash()) -> calendar:datetime().
updated_at(#{updated_at := UpdatedAt}) -> UpdatedAt.
