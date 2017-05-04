-module(canillita_news_SUITE).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ canillita_test_utils
        , [ init_per_suite/1
          , end_per_suite/1
          ]
        }]).

-export([ all/0
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([news_api_test/1, last_event_id_test/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% common tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() -> canillita_test_utils:all(?MODULE).

-spec init_per_testcase( TestCase::atom()
                       , Config::canillita_test_utils:config()
                       ) -> canillita_test_utils:config().
init_per_testcase(_TestCase, Config) ->
  _ = sumo:delete_all(canillita_newspapers),
  _ = sumo:delete_all(canillita_newsitems),
  Config.

-spec end_per_testcase( TestCase::atom()
                      , Config::canillita_test_utils:config()
                      ) -> canillita_test_utils:config().
end_per_testcase(_TestCase, Config) ->
  Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec news_api_test(Config::canillita_test_utils:config()) ->
  {comment, string()}.
news_api_test(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json; charset=utf-8">>},
  % Every newsitem needs to be attached to an existing newspaper
  Newspaper = canillita_test_utils:create_newspaper( <<"newspaper1">>,
                                                      <<"description1">>
                                                    ),
  NewspaperBin = list_to_binary(Newspaper),
  Newspaper2 = canillita_test_utils:create_newspaper( <<"newspaper2">>
                                                    , <<"description2">>
                                                    ),
  NewspaperBin2 = list_to_binary(Newspaper2),
  % open SSE connection
  AsyncPid = canillita_test_utils:async_api_call("/news", Headers),
  % create a newsitem
  Body1 = #{<<"title">> => <<"title1">> , <<"body">> => <<"body1">>},
  #{status_code := 201, body := RespBody1} =
    canillita_test_utils:create_newsitem(Newspaper, Headers, Body1),
  Id1 = maps:get(<<"id">>, sr_json:decode(RespBody1)),
  % check it matches the expected response
  ParsedEvent1 =
    #{ id => Id1
     , event => NewspaperBin
     , data => [<<"title1">>, <<"body1">>]
     },
  ParsedEvent1 = wait_for_parsed_event(AsyncPid, ParsedEvent1),
  Id1 = maps:get(id, ParsedEvent1),

  % create a new newsitem
  Body2 = #{<<"title">> => <<"title2">> , <<"body">> => <<"body2">>},
  #{status_code := 201, body := RespBody2} =
    canillita_test_utils:create_newsitem(Newspaper, Headers, Body2),
  Id2 = maps:get(<<"id">>, sr_json:decode(RespBody2)),
  % check it matches the expected response
  ParsedEvent2 =
    #{ id => Id2
     , event => NewspaperBin
     , data => [<<"title2">>, <<"body2">>]
     },
  ParsedEvent2 = wait_for_parsed_event(AsyncPid, ParsedEvent2),
  Id2 = maps:get(id, ParsedEvent2),

  %create a new newsitem for a different newspaper
  Body3 = #{<<"title">> => <<"title3">>, <<"body">> => <<"body3">>},
  #{status_code := 201, body := RespBody3} =
    canillita_test_utils:create_newsitem(Newspaper2, Headers, Body3),
  Id3 = maps:get(<<"id">>, sr_json:decode(RespBody3)),
  % and check it is also been notified
  ParsedEvent3 =
    #{ id => Id3
     , event => NewspaperBin2
     , data => [<<"title3">>, <<"body3">>]
     },
  ParsedEvent3 = wait_for_parsed_event(AsyncPid, ParsedEvent3),
  Id3 = maps:get(id, ParsedEvent3),

  % close shotgun connection
  shotgun:close(AsyncPid),

  {comment, ""}.

-spec last_event_id_test(Config::canillita_test_utils:config()) ->
  {comment, string()}.
last_event_id_test(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json; charset=utf-8">>},
  % Every newsitem needs to be attached to an existing newspaper
  Newspaper = canillita_test_utils:create_newspaper( <<"newspaper1">>,
                                                      <<"description1">>
                                                    ),
  NewspaperBin = list_to_binary(Newspaper),

  % create a newsitem0
  Body0 = #{<<"title">> => <<"title0">> , <<"body">> => <<"body0">>},
  #{status_code := 201, body := _RespBody0} =
    canillita_test_utils:create_newsitem(Newspaper, Headers, Body0),
  % create a newsitem1
  Body1 = #{<<"title">> => <<"title1">> , <<"body">> => <<"body1">>},
  #{status_code := 201, body := RespBody1} =
    canillita_test_utils:create_newsitem(Newspaper, Headers, Body1),
  Id1 = maps:get(<<"id">>, sr_json:decode(RespBody1)),

  % create header for getting just the events from now on
  LastEventIdHeaders = Headers#{<<"last-event-id">> => Id1},
  % open SSE connection with last-event-id header set
  AsyncPid = canillita_test_utils:async_api_call("/news", LastEventIdHeaders),

  % create a newsitem2
  Body2 = #{<<"title">> => <<"title2">> , <<"body">> => <<"body2">>},
  #{status_code := 201, body := RespBody2} =
    canillita_test_utils:create_newsitem(Newspaper, Headers, Body2),
  Id2 = maps:get(<<"id">>, sr_json:decode(RespBody2)),

  % check it matches the expected response
  ParsedEvent2 =
    #{ id => Id2
     , event => NewspaperBin
     , data => [<<"title2">>, <<"body2">>]
     },
  % only the last newsitem must be returned
  ParsedEvent2 = wait_for_parsed_event(AsyncPid, ParsedEvent2),
  Id2 = maps:get(id, ParsedEvent2),

  % close SSE connection
  shotgun:close(AsyncPid),

  {comment, ""}.

wait_for_parsed_event(AsyncPid, ParsedEvent) ->
  ktn_task:wait_for(
    fun() ->
        [{_, _, EventBin}] = shotgun:events(AsyncPid),
        Event = shotgun:parse_event(EventBin),
        #{data := AllData} = Event,
        Event#{data := binary:split(AllData, <<"\n">>, [global, trim])}
    end, ParsedEvent, 300, 10
  ).
