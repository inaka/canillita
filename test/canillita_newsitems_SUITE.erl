-module(canillita_newsitems_SUITE).

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

-export([ success_scenario/1
        , invalid_headers/1
        , invalid_parameters/1
        , not_found/1
        ]).


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

-spec success_scenario(Config::canillita_test_utils:config()) ->
  {comment, string()}.
success_scenario(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json; charset=utf-8">>},
  % Every newsitem needs to be attached to an existing newspaper
  Newspaper1 = create_newspaper(<<"newspaper1">>, <<"description1">>),
  Newspaper1Bin = list_to_binary(Newspaper1),
  Newspaper2 = create_newspaper(<<"newspaper2">>, <<"description2">>),
  Newspaper2Bin = list_to_binary(Newspaper2),

  ct:comment("Create a newsitem"),
  #{status_code := 201, body := Body1} =
    canillita_test_utils:api_call(
      post
    , "/newspapers/" ++ Newspaper1 ++ "/news"
    , Headers
    , #{ <<"title">> => <<"title1">>
       , <<"body">> => <<"body1">>
       }
    ),
  #{ <<"id">> := NewsItem1Id
   , <<"newspaper_name">> := Newspaper1Bin
   , <<"title">> := <<"title1">>
   , <<"body">> := <<"body1">>
   , <<"created_at">> := _CreatedAt
   } = sr_json:decode(Body1),

  ct:comment("And we can fetch it"),
  NewsItem1Url = "/newspapers/" ++ Newspaper1 ++ "/news/" ++ NewsItem1Id,
  #{status_code := 200} = canillita_test_utils:api_call(get, NewsItem1Url),

  ct:comment("Create a new newsitem"),
  #{status_code := 201, body := Body2} =
    canillita_test_utils:api_call(
      post
    , "/newspapers/" ++ Newspaper2 ++ "/news"
    , Headers
    , #{ <<"title">> => <<"title2">>
       , <<"body">> => <<"body2">>
       }
    ),
  #{ <<"id">> := NewsItem2Id
   , <<"newspaper_name">> := Newspaper2Bin
   , <<"title">> := <<"title2">>
   , <<"body">> := <<"body2">>
   , <<"created_at">> := _CreatedAt
   } = sr_json:decode(Body2),

  ct:comment("And we can fetch it too"),
  NewsItem2Url = "/newspapers/" ++ Newspaper2 ++ "/news/" ++ NewsItem2Id,
  #{status_code := 200} = canillita_test_utils:api_call(get, NewsItem2Url),

  {comment, ""}.

-spec invalid_headers(Config::canillita_test_utils:config()) ->
  {comment, string()}.
invalid_headers(_Config) ->
  NoHeaders = #{},
  InvalidHeaders = #{<<"content-type">> => <<"text/plain">>},
  InvalidAccept = #{ <<"content-type">> => <<"application/json">>
                   , <<"accept">> => <<"text/html">>
                   },
  Newspaper1 = create_newspaper(<<"newspaper1">>, <<"description1">>),
  NewsItem1Url = "/newspapers/" ++ Newspaper1 ++ "/news",
  ct:comment("content-type must be provided for POST"),
  #{status_code := 415} =
    canillita_test_utils:api_call(post, NewsItem1Url, NoHeaders, <<>>),

  ct:comment("content-type must be JSON for POST"),
  #{status_code := 415} =
    canillita_test_utils:api_call(post, NewsItem1Url, InvalidHeaders, <<>>),

  ct:comment("Agent must accept JSON for POST"),
  #{status_code := 406} =
    canillita_test_utils:api_call(post, NewsItem1Url, InvalidAccept, <<>>),

  {comment, ""}.

-spec invalid_parameters(Config::canillita_test_utils:config()) ->
  {comment, string()}.
invalid_parameters(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json">>},
  Newspaper1 = create_newspaper(<<"newspaper1">>, <<"description1">>),
  NewsItem1Url = "/newspapers/" ++ Newspaper1 ++ "/news",

  ct:comment("Empty or broken parameters are reported"),
  #{status_code := 400} =
    canillita_test_utils:api_call(post, NewsItem1Url, Headers, <<>>),
  #{status_code := 400} =
    canillita_test_utils:api_call(post, NewsItem1Url, Headers, <<"{">>),

  ct:comment("Missing parameters are reported"),
  None = #{},
  #{status_code := 400} =
    canillita_test_utils:api_call(post, NewsItem1Url, Headers, None),

  NoTitle = #{body => <<"notitle">>},
  #{status_code := 400} =
    canillita_test_utils:api_call(post, NewsItem1Url, Headers, NoTitle),

  NoBody = #{title => <<"nobody">>},
  #{status_code := 400} =
    canillita_test_utils:api_call(post, NewsItem1Url, Headers, NoBody),

  {comment, ""}.

-spec not_found(Config::canillita_test_utils:config()) -> {comment, string()}.
not_found(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json; charset=utf-8">>},
  NewsItem = #{title => <<"titlex">>, body => <<"bodyx">>},
  ct:comment("Unable to create a newsitem with a non-existing newspaper"),
  #{status_code := 404} =
    canillita_test_utils:api_call( post
                                 , "/newspapers/non-existing/news"
                                 , Headers
                                 , NewsItem
                                 ),

  Newspaper1 = create_newspaper(<<"newspaper1">>, <<"description1">>),
  Newspaper1Bin = list_to_binary(Newspaper1),
  NewsItem1Url = "/newspapers/" ++ Newspaper1 ++ "/news",

  ct:comment("Create a newsitem"),
  #{status_code := 201, body := Body1} =
    canillita_test_utils:api_call(
      post
    , "/newspapers/" ++ Newspaper1 ++ "/news"
    , Headers
    , #{ <<"title">> => <<"titlenf1">>
       , <<"body">> => <<"bodynf1">>
       }
    ),
  #{ <<"id">> := NewsItem1Id
   , <<"newspaper_name">> := Newspaper1Bin
   , <<"title">> := <<"titlenf1">>
   , <<"body">> := <<"bodynf1">>
   , <<"created_at">> := _CreatedAt
   } = sr_json:decode(Body1),

  ct:comment("Unable to get non-existing newsitem with a valid newspaper"),
  #{status_code := 404} =
    canillita_test_utils:api_call(get, NewsItem1Url ++ "/non-existing-id"),

  ct:comment("Unable to get existing newsitem id with invalid newspaper"),
  #{status_code := 404} =
    canillita_test_utils:api_call(
      get
    , "/newspapers/non-existing-newspaper/news/" ++ NewsItem1Id
    ),

  {comment, ""}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec create_newspaper( Name::canillita_newspapers:name()
                      , Description::canillita_newspapers:description()
                      ) -> iodata().
create_newspaper(Name, Description) ->
  Headers = #{<<"content-type">> => <<"application/json; charset=utf-8">>},
  #{status_code := 201, body := NewspaperBody} =
    canillita_test_utils:api_call(
      post
    , "/newspapers"
    , Headers
    , #{ name => Name
       , description => Description
       }
    ),
  Newspaper = sr_json:decode(NewspaperBody),
  Name = maps:get(<<"name">>, Newspaper),
  binary_to_list(Name).
