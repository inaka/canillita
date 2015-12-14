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

-export([news_api_test/1]).

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
  AsyncPid = canillita_test_utils:async_api_call("/news", Headers),
  ok = try
    % create a newsitem
    Body1 = #{<<"title">> => <<"title1">> , <<"body">> => <<"body1">>},
    #{status_code := 201, body := RespBody1} =
      canillita_test_utils:create_newsitem(Newspaper, Headers, Body1),
    Id1 = maps:get(<<"id">>, sr_json:decode(RespBody1)),
    % Wait the event replication
    timer:sleep(300),
    [{_, _, EventBin1}] = shotgun:events(AsyncPid),
    Id1 = maps:get(id, shotgun:parse_event(EventBin1)),

    % create a new newsitem
    Body2 = #{<<"title">> => <<"title2">> , <<"body">> => <<"body2">>},
    #{status_code := 201, body := RespBody2} =
      canillita_test_utils:create_newsitem(Newspaper, Headers, Body2),
    Id2 = maps:get(<<"id">>, sr_json:decode(RespBody2)),
    % Wait the event replication
    timer:sleep(300),
    [{_, _, EventBin2}] = shotgun:events(AsyncPid),
    Id2 = maps:get(id, shotgun:parse_event(EventBin2)),
    ok
  catch
    _:Exception -> throw({error, Exception})
  after
    shotgun:close(AsyncPid)
  end,

  {comment, ""}.
