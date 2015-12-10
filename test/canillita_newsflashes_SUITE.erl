-module(canillita_newsflashes_SUITE).

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
  _ = sumo:delete_all(canillita_newsflashes),
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
  % Every newsflash needs to be attached to an existing newspaper
  Newspaper1 = create_newspaper(<<"newspaper1">>, <<"description1">>),
  Newspaper1Bin = list_to_binary(Newspaper1),
  Newspaper2 = create_newspaper(<<"newspaper2">>, <<"description2">>),
  Newspaper2Bin = list_to_binary(Newspaper2),

  ct:comment("Create a newsflash"),
  #{status_code := 201, body := Body1} =
    canillita_test_utils:api_call(
      post
    , "/newspapers/"++Newspaper1++"/news"
    , Headers
    , #{ <<"title">> => <<"title1">>
       , <<"body">> => <<"body1">>
       }
    ),
  #{ <<"id">> := NewsFlash1Id
   , <<"newspaper_name">> := Newspaper1Bin
   , <<"title">> := <<"title1">>
   , <<"body">> := <<"body1">>
   , <<"created_at">> := CreatedAt
   , <<"updated_at">> := CreatedAt
   } = sr_json:decode(Body1),

  ct:comment("And we can fetch it"),
  NewsFlash1Url = "/newspapers/"++Newspaper1++"/news/"++NewsFlash1Id,
  #{status_code := 200} = canillita_test_utils:api_call(get, NewsFlash1Url),

  ct:comment("Create a new newsflash"),
  #{status_code := 201, body := Body2} =
    canillita_test_utils:api_call(
      post
    , "/newspapers/"++Newspaper2++"/news"
    , Headers
    , #{ <<"title">> => <<"title2">>
       , <<"body">> => <<"body2">>
       }
    ),
  #{ <<"id">> := NewsFlash2Id
   , <<"newspaper_name">> := Newspaper2Bin
   , <<"title">> := <<"title2">>
   , <<"body">> := <<"body2">>
   , <<"created_at">> := CreatedAt2
   , <<"updated_at">> := CreatedAt2
   } = sr_json:decode(Body2),

  ct:comment("And we can fetch it too"),
  NewsFlash2Url = "/newspapers/"++Newspaper2++"/news/"++NewsFlash2Id,
  #{status_code := 200} = canillita_test_utils:api_call(get, NewsFlash2Url),

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
  NewsFlash1Url = "/newspapers/"++Newspaper1++"/news",
  ct:comment("content-type must be provided for POST"),
  #{status_code := 415} =
    canillita_test_utils:api_call(post, NewsFlash1Url, NoHeaders, <<>>),

  ct:comment("content-type must be JSON for POST"),
  #{status_code := 415} =
    canillita_test_utils:api_call(post, NewsFlash1Url, InvalidHeaders, <<>>),

  ct:comment("Agent must accept JSON for POST"),
  #{status_code := 406} =
    canillita_test_utils:api_call(post, NewsFlash1Url, InvalidAccept, <<>>),

  {comment, ""}.

-spec invalid_parameters(Config::canillita_test_utils:config()) ->
  {comment, string()}.
invalid_parameters(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json">>},
  Newspaper1 = create_newspaper(<<"newspaper1">>, <<"description1">>),
  NewsFlash1Url = "/newspapers/"++Newspaper1++"/news",

  ct:comment("Empty or broken parameters are reported"),
  #{status_code := 400} =
    canillita_test_utils:api_call(post, NewsFlash1Url, Headers, <<>>),
  #{status_code := 400} =
    canillita_test_utils:api_call(post, NewsFlash1Url, Headers, <<"{">>),

  ct:comment("Missing parameters are reported"),
  None = #{},
  #{status_code := 400} =
    canillita_test_utils:api_call(post, NewsFlash1Url, Headers, None),

  NoTitle = #{body => <<"notitle">>},
  #{status_code := 400} =
    canillita_test_utils:api_call(post, NewsFlash1Url, Headers, NoTitle),

  NoBody = #{title => <<"nobody">>},
  #{status_code := 400} =
    canillita_test_utils:api_call(post, NewsFlash1Url, Headers, NoBody),

  {comment, ""}.

-spec not_found(Config::canillita_test_utils:config()) -> {comment, string()}.
not_found(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json; charset=utf-8">>},
  NewsFlash = #{title => <<"titlex">>, body => <<"bodyx">>},
  ct:comment("Unable to create a newsflash with a non-existing newspaper"),
  #{status_code := 404} =
    canillita_test_utils:api_call( post
                                 , "/newspapers/non-existing/news"
                                 , Headers
                                 , NewsFlash
                                 ),

  Newspaper1 = create_newspaper(<<"newspaper1">>, <<"description1">>),
  NewsFlash1Url = "/newspapers/"++Newspaper1++"/news",

  ct:comment("Unable to get non-existing newsflash"),
  #{status_code := 404} =
    canillita_test_utils:api_call( get
                                 , NewsFlash1Url++"/non-existing-id"
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
