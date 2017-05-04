%%% @doc NewsItem Model
-module(canillita_newsitems).

-behaviour(sumo_doc).
-behaviour(sumo_rest_doc).

-type id() :: binary().
-type newspaper_name() :: canillita_newspapers:name().
-type title() :: binary().
-type body() :: binary().

-opaque news_item() ::
  #{ id => id() | undefined
   , newspaper_name => newspaper_name()
   , title => title()
   , body => body()
   , created_at => calendar:datetime()
   }.

-export_type(
  [ id/0
  , newspaper_name/0
  , title/0
  , body/0
  , news_item/0
  ]).

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
  , location/2
  ]).

%% Public API
-export([to_sse/1]).

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
    ]).

%% @doc Convert a newspaper from its system representation to sumo's
%%      internal one.
-spec sumo_sleep(NewsItem::news_item()) -> sumo:model().
sumo_sleep(NewsItem) -> NewsItem.

%% @doc Convert a newspaper from sumo's internal representation to its
%%      system one.
-spec sumo_wakeup(NewsItem::sumo:doc()) -> news_item().
sumo_wakeup(NewsItem) -> NewsItem.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_rest_doc behaviour callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert a newspaper from its system representation to json.
-spec to_json(NewsItem::news_item()) -> sr_json:json().
to_json(NewsItem) ->
  #{ id => sr_json:encode_null(maps:get(id, NewsItem))
   , newspaper_name => maps:get(newspaper_name, NewsItem)
   , title => maps:get(title, NewsItem)
   , body => maps:get(body, NewsItem)
   , created_at => sr_json:encode_date(maps:get(created_at, NewsItem))
   }.

%% @doc Convert a newspaper from json to its system representation.
-spec from_json(NewspaperName::newspaper_name(), sumo_rest_doc:json()) ->
  {ok, news_item()} | {error, iodata()}.
from_json(NewspaperName, Json) ->
  from_json(Json#{<<"newspaper_name">> => NewspaperName}).

-spec from_json(Json::sumo_rest_doc:json()) ->
  {ok, news_item()} | {error, iodata()}.
from_json(Json) ->
  Now = sr_json:encode_date(calendar:universal_time()),
  try
    { ok
    , #{ id => sr_json:decode_null(maps:get(<<"id">>, Json, null))
       , newspaper_name => maps:get(<<"newspaper_name">>, Json)
       , title => maps:get(<<"title">>, Json)
       , body => maps:get(<<"body">>, Json)
       , created_at =>
          sr_json:decode_date(maps:get(<<"created_at">>, Json, Now))
       }
    }
  catch
    _:{badkey, Key} -> {error, <<"missing field: ", Key/binary>>}
  end.

-spec update(NewsItem::news_item(), Json::sumo_rest_doc:json()) ->
  {ok, news_item()}.
update(NewsItem, _Json) -> {ok, NewsItem}.

%% @doc Specify the URL that identifies a NewsItem.
-spec location(NewsItem::news_item(), Path::sumo_rest_doc:path()) -> iodata().
location(#{id := NewsId, newspaper_name := NewspaperName}, _Path) ->
  iolist_to_binary(["/newspapers/", NewspaperName, "/news/", NewsId]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert a newspaper from its system representation to SSE.
-spec to_sse(NewsItem::news_item()) -> lasse_handler:event().
to_sse(NewsItem) ->
  #{ id => maps:get(id, NewsItem)
   , event => maps:get(newspaper_name, NewsItem)
   , data => iolist_to_binary([ maps:get(title, NewsItem)
                              , "\n"
                              , maps:get(body, NewsItem)
                              ])
   }.
