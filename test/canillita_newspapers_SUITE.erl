-module(canillita_newspapers_SUITE).

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
        , duplicated_name/1
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

  ct:comment("There are no newspapers"),
  #{status_code := 200, body := Body0} =
    canillita_test_utils:api_call(get, "/newspapers"),
  [] = sr_json:decode(Body0),

  ct:comment("Newspaper 1 is created"),
  #{status_code := 201, body := Body1} =
    canillita_test_utils:api_call(
      post
    , "/newspapers"
    , Headers
    , #{ name => <<"newspaper1">>
       , description => <<"description1">>
       }
    ),
  #{ <<"name">> := <<"newspaper1">>
   , <<"description">> := <<"description1">>
   , <<"created_at">> := CreatedAt
   , <<"updated_at">> := CreatedAt
   } = Newspaper1 = sr_json:decode(Body1),

  ct:comment("There is one newspaper now"),
  #{status_code := 200, body := Body2} =
    canillita_test_utils:api_call(get, "/newspapers"),
  [Newspaper1] = sr_json:decode(Body2),

  ct:comment("And we can fetch it (~p)", [Newspaper1]),
  #{status_code := 200, body := Body21} =
    canillita_test_utils:api_call(get, "/newspapers/newspaper1"),
  Newspaper1 = sr_json:decode(Body21),

  ct:comment("The newspaper description can be changed"),
  #{status_code := 200, body := Body3} =
  canillita_test_utils:api_call(
    put
  , "/newspapers/newspaper1"
  , Headers
  , #{ name => <<"newspaper1">>
     , description => <<"newdescription3">>
     }
  ),
  #{ <<"name">> := <<"newspaper1">>
   , <<"description">> := <<"newdescription3">>
   , <<"created_at">> := CreatedAt
   , <<"updated_at">> := UpdatedAt
   } = Newspaper3 = sr_json:decode(Body3),
  true = UpdatedAt >= CreatedAt,

  ct:comment("Still just one newspaper"),
  #{status_code := 200, body := Body4} =
    canillita_test_utils:api_call(get, "/newspapers"),
  [Newspaper3] = sr_json:decode(Body4),

  ct:comment("Newspapers can be created by PUT"),
  #{status_code := 201, body := Body5} =
  canillita_test_utils:api_call(
    put
  , "/newspapers/newspaper2"
  , Headers
  , #{ name => <<"newspaper2">>
     , description => <<"description2">>
     }
  ),
  #{ <<"name">> := <<"newspaper2">>
   , <<"description">> := <<"description2">>
   , <<"created_at">> := CreatedAt4
   , <<"updated_at">> := CreatedAt4
   } = Newspaper4 = sr_json:decode(Body5),
  true = CreatedAt4 >= CreatedAt,

  ct:comment("There are two newspapers now"),
  #{status_code := 200, body := Body6} =
    canillita_test_utils:api_call(get, "/newspapers"),
  [Newspaper3, Newspaper4] = sr_json:decode(Body6),

  ct:comment("Newspaper1 is deleted"),
  #{status_code := 204} =
    canillita_test_utils:api_call(delete, "/newspapers/newspaper1"),

  ct:comment("One newspaper again"),
  #{status_code := 200, body := Body7} =
    canillita_test_utils:api_call(get, "/newspapers"),
  [Newspaper4] = sr_json:decode(Body7),

  ct:comment("DELETE is not idempotent"),
  #{status_code := 204} =
    canillita_test_utils:api_call(delete, "/newspapers/newspaper2"),
  #{status_code := 404} =
    canillita_test_utils:api_call(delete, "/newspapers/newspaper2"),

  ct:comment("There are no newspapers"),
  #{status_code := 200, body := Body8} =
    canillita_test_utils:api_call(get, "/newspapers"),
  [] = sr_json:decode(Body8),

  {comment, ""}.

-spec duplicated_name(Config::canillita_test_utils:config()) ->
  {comment, string()}.
duplicated_name(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json; charset=utf-8">>},
  Body =
    #{ name => <<"newspaper1">>
     , description => <<"description1">>
     },

  ct:comment("Newspaper1 is created"),
  #{status_code := 201} =
    canillita_test_utils:api_call(post, "/newspapers", Headers, Body),

  ct:comment("Newspaper1 can't be created again"),
  #{status_code := 422} =
    canillita_test_utils:api_call(post, "/newspapers", Headers, Body),

  {comment, ""}.

-spec invalid_headers(Config::canillita_test_utils:config()) ->
  {comment, string()}.
invalid_headers(_Config) ->
  NoHeaders = #{},
  InvalidHeaders = #{<<"content-type">> => <<"text/plain">>},
  InvalidAccept = #{ <<"content-type">> => <<"application/json">>
                   , <<"accept">> => <<"text/html">>
                   },
  ct:comment("content-type must be provided for POST and PUT"),
  #{status_code := 415} =
    canillita_test_utils:api_call(post, "/newspapers", NoHeaders, <<>>),
  #{status_code := 415} =
    canillita_test_utils:api_call( put
                                 , "/newspapers/noheaders"
                                 , NoHeaders
                                 , <<>>
                                 ),

  ct:comment("content-type must be JSON for POST and PUT"),
  #{status_code := 415} =
    canillita_test_utils:api_call(post, "/newspapers", InvalidHeaders, <<>>),
  #{status_code := 415} =
    canillita_test_utils:api_call( put
                                 , "/newspapers/badtype"
                                 , InvalidHeaders
                                 , <<>>
                                 ),

  ct:comment("Agent must accept JSON for POST, GET and PUT"),
  #{status_code := 406} =
    canillita_test_utils:api_call(post, "/newspapers", InvalidAccept, <<>>),
  #{status_code := 406} =
    canillita_test_utils:api_call(get, "/newspapers", InvalidAccept, <<>>),
  #{status_code := 406} =
    canillita_test_utils:api_call( put
                                 , "/newspapers/badaccept"
                                 , InvalidAccept
                                 , <<>>
                                 ),
  #{status_code := 406} =
    canillita_test_utils:api_call( get
                                 , "/newspapers/badaccept"
                                 , InvalidAccept
                                 , <<>>
                                 ),

  {comment, ""}.

-spec invalid_parameters(Config::canillita_test_utils:config()) ->
  {comment, string()}.
invalid_parameters(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json">>},
  _ = sumo:persist( canillita_newspapers
                  , canillita_newspapers:new(<<"name">>, <<"description">>)
                  ),

  ct:comment("Empty or broken parameters are reported"),
  #{status_code := 400} =
    canillita_test_utils:api_call(post, "/newspapers", Headers, <<>>),
  #{status_code := 400} =
    canillita_test_utils:api_call(put, "/newspapers/nobody", Headers, <<>>),
  #{status_code := 400} =
    canillita_test_utils:api_call(put, "/newspapers/name", Headers, <<>>),
  #{status_code := 400} =
    canillita_test_utils:api_call(post, "/newspapers", Headers, <<"{">>),
  #{status_code := 400} =
    canillita_test_utils:api_call(put, "/newspapers/broken", Headers, <<"{">>),
  #{status_code := 400} =
    canillita_test_utils:api_call(put, "/newspapers/name", Headers, <<"{">>),

  ct:comment("Missing parameters are reported"),
  None = #{},
  #{status_code := 400} =
    canillita_test_utils:api_call(post, "/newspapers", Headers, None),
  #{status_code := 400} =
    canillita_test_utils:api_call(put, "/newspapers/none", Headers, None),
  #{status_code := 400} =
    canillita_test_utils:api_call(put, "/newspapers/name", Headers, None),

  NoDesc = #{name => <<"nodesc">>},
  #{status_code := 400} =
    canillita_test_utils:api_call(post, "/newspapers", Headers, NoDesc),
  #{status_code := 400} =
    canillita_test_utils:api_call(put, "/newspapers/nodesc", Headers, NoDesc),
  #{status_code := 400} =
    canillita_test_utils:api_call(put, "/newspapers/name", Headers, NoDesc),

  {comment, ""}.

-spec not_found(Config::canillita_test_utils:config()) -> {comment, string()}.
not_found(_Config) ->
  ct:comment("Not existing newspaper is not found"),
  #{status_code := 404} =
    canillita_test_utils:api_call(get, "/newspapers/notfound"),
  #{status_code := 404} =
    canillita_test_utils:api_call(delete, "/newspapers/notfound"),

  {comment, ""}.
