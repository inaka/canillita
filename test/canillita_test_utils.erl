%%% @doc General Canillita test utils
-module(canillita_test_utils).

-export([ all/1
        , init_per_suite/1
        , end_per_suite/1
        ]).
-export([ api_call/2
        , api_call/3
        , api_call/4
        ]).
-export([async_api_call/2, create_newspaper/2, create_newsitem/3]).

-type config() :: proplists:proplist().
-export_type([config/0]).

-type method() :: atom().
-type uri() :: string().
-type headers() :: map().
-type body() :: map() | iodata().

-spec all(Module::atom()) -> [atom()].
all(Module) ->
  ExcludedFuns = [module_info, init_per_suite, end_per_suite, group, all],
  Exports = Module:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_suite(Config::config()) -> config().
init_per_suite(Config) ->
  {ok, _} = canillita:start(),
  {ok, _} = shotgun:start(),
  Config.

-spec end_per_suite(Config::config()) -> config().
end_per_suite(Config) ->
  _ = canillita:stop(),
  _ = shotgun:stop(),
  Config.

-spec api_call(Method::method(), Uri::uri()) -> map().
api_call(Method, Uri) ->
  api_call(Method, Uri, #{}).

-spec api_call(Method::method(), Uri::uri(), Headers::headers()) -> map().
api_call(Method, Uri, Headers) ->
  api_call(Method, Uri, Headers, []).

-spec api_call( Method::method()
              , Uri::uri()
              , Headers::headers()
              , Body::body()
              ) -> map().
api_call(Method, Uri, Headers, Body) when is_map(Body) ->
  api_call(Method, Uri, Headers, sr_json:encode(Body));
api_call(Method, Uri, Headers, Body) ->
  {ok, Pid} = shotgun:open("localhost", 4892),
  try
    Options = #{},
    {ok, Response} = shotgun:request(Pid, Method, Uri, Headers, Body, Options),
    Response
  after
    shotgun:close(Pid)
  end.

-spec async_api_call(Uri::uri(), Headers::headers()) -> pid().
async_api_call(Uri, Headers) ->
  {ok, Pid} = shotgun:open("localhost", 4892),
  {ok, _} = try
    Options = #{async => true, async_mode => sse},
    {ok, _Ref} = shotgun:get(Pid, Uri, Headers, Options)
  catch
    _:Exception -> exit({error, Exception})
  end,
  Pid.

-spec create_newspaper( Name::canillita_newspapers:name()
                      , Description::canillita_newspapers:description()
                      ) -> iodata().
create_newspaper(Name, Description) ->
  Headers = #{<<"content-type">> => <<"application/json; charset=utf-8">>},
  #{status_code := 201, body := NewspaperBody} =
    api_call(
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

-spec create_newsitem( Newspaper::string()
                     , Headers::headers()
                     , Body::body()
                     ) -> map().
create_newsitem(Newspaper, Headers, Body) ->
  api_call(
    post
  , "/newspapers/" ++ Newspaper ++ "/news"
  , Headers
  , Body
  ).
