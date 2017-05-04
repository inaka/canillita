%%% @doc POST /newspapers/:name/news handler.
-module(canillita_newsitems_handler).

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_entities_handler
        , [ init/3
          , rest_init/2
          , resource_exists/2
          , allowed_methods/2
          , announce_req/2
          , content_types_accepted/2
          , content_types_provided/2
          , handle_post/3
          ]
        }]).

%% Alias
-type state() :: sr_entities_handler:state().

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
       #{ tags => ["newsitems"]
        , description => "Creates a new news item"
        , consumes => ["application/json", "application/json; charset=utf-8"]
        , produces => ["application/json"]
        , parameters => [NewspaperName, RequestBody]
        }
     },
  Path = "/newspapers/:name/news",
  Options = #{path => Path, model => canillita_newsitems, verbose => true},
  [trails:trail(Path, ?MODULE, Options, Metadata)].

-spec handle_post(Req::cowboy_req:req(), State::state()) ->
  { {true, binary()} | false | halt
  , cowboy_req:req()
  , state()
  }.
handle_post(Req, State) ->
  try
    {ok, Body, Req1}      = cowboy_req:body(Req),
    Json                  = sr_json:decode(Body),
    {NewspaperName, _Req} = cowboy_req:binding(name, Req),
    % Checks that the given newspaper does exists
    case canillita_newspapers_repo:exists(NewspaperName) of
      true ->
        case canillita_newsitems:from_json(NewspaperName, Json) of
          {error, Reason} ->
            Req2 = cowboy_req:set_resp_body(sr_json:error(Reason), Req1),
            {false, Req2, State};
          {ok, Entity} ->
            handle_post(Entity, Req1, State)
        end;
      false ->
        {ok, Req1} = cowboy_req:reply(404, Req),
        {halt, Req1, State}
    end
  catch
    _:badjson ->
      Req3 =
        cowboy_req:set_resp_body(
          sr_json:error(<<"Malformed JSON request">>), Req),
      {false, Req3, State}
  end.
