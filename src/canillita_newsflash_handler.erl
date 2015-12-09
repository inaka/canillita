%%% @doc POST /newspapers/:name/news handler.
-module(canillita_newsflash_handler).

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_entities_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , resource_exists/2
          , content_types_accepted/2
          , content_types_provided/2
          , handle_get/2
          , handle_post/3
          ]
        }]).

-export([trails/0, handle_post/2]).

-spec trails() -> trails:trails().
trails() ->
  NewspaperName =
    #{ name => name
     , in => path
     , description => <<"Newspaper name">>
     , required => true
     , type => string
     },
  RequestBody =
    #{ name => <<"request body">>
     , in => body
     , description => <<"request body (as json)">>
     , required => true
     },
  Metadata =
    #{ post =>
       #{ tags => ["newsflashes"]
        , description => "Creates a new news flash"
        , consumes => ["application/json"]
        , produces => ["application/json"]
        , parameters => [NewspaperName, RequestBody]
        }
     },
  Path = "/newspapers/:name/news",
  Options = #{path => Path, model => canillita_newsflash, verbose => true},
  [trails:trail(Path, ?MODULE, Options, Metadata)].

-spec handle_post(cowboy_req:req(), sr_entities_handler:state()) ->
  {{true, binary()} |
   false |
   halt, cowboy_req:req(), sr_entities_handler:state()}.
handle_post(Req, State) ->
  #{opts := #{model := Model}} = State,
  try
    {ok, Body, Req1}      = cowboy_req:body(Req),
    Json                  = sr_json:decode(Body),
    {NewspaperName, _Req} = cowboy_req:binding(name, Req),
    case Model:from_json(NewspaperName, Json) of
      {error, Reason} ->
        Req2 = cowboy_req:set_resp_body(sr_json:error(Reason), Req1),
        {false, Req2, State};
      {ok, Entity} ->
        handle_post(Entity, Req1, State)
    end
  catch
    _:conflict ->
      {ok, Req3} =
        cowboy_req:reply(409, [], sr_json:error(<<"Duplicated entity">>), Req),
      {halt, Req3, State};
    _:badjson ->
      Req3 =
        cowboy_req:set_resp_body(
          sr_json:error(<<"Malformed JSON request">>), Req),
      {false, Req3, State}
  end.
