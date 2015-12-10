%%% @doc GET /newspapers/:name/news/:id handler.
-module(canillita_single_newsflash_handler).

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_single_entity_handler
        , [ init/3
          , allowed_methods/2
          , content_types_accepted/2
          , content_types_provided/2
          , handle_get/2
          ]
        }]).
-mixin([{ sr_entities_handler, [announce_req/2] }]).

%% Aliases
-type state() :: sr_single_entity_handler:state().
-type options() :: sr_single_entity_handler:options().

-export([ trails/0
        , rest_init/2
        , resource_exists/2
        ]).

-spec trails() -> trails:trails().
trails() ->
  NewspaperName =
    #{ name => name
     , in => path
     , description => <<"Newspaper name">>
     , required => true
     , type => string
     },
  NewsFlashId =
    #{ name => id
     , in => path
     , descripcion => <<"News flash id">>
     , required => true
     , type => string
     },
  Metadata =
    #{ get =>
       #{ tags => ["newsflashes"]
        , description => "Return a newsflash"
        , produces => ["application/json"]
        , parameters => [NewspaperName, NewsFlashId]
        }
     },
  Path = "/newspapers/:name/news/:id",
  Options = #{path => Path, model => canillita_newsflashes},
  [trails:trail(Path, ?MODULE, Options, Metadata)].

-spec rest_init( Req::cowboy_req:req(), Options::options()) ->
  {ok, cowboy_req:req(), state()}.
rest_init(Req, Options) ->
  Req1 = announce_req(Req, Options),
  {Id, Req2} = cowboy_req:binding(id, Req1),
  {NewspaperName, Req3} = cowboy_req:binding(name, Req2),
  {ok, Req3, #{opts => Options, id => Id, newspaper_name => NewspaperName}}.

-spec resource_exists( Req::cowboy_req:req(), State::state()) ->
  {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, State) ->
  #{opts := #{model := Model}, id := Id, newspaper_name := NewspaperName} =
    State,
  Conditions = [{id, Id}, {newspaper_name, NewspaperName}],
  case sumo:find_one(Model, Conditions) of
    notfound -> {false, Req, State};
    Entity -> {true, Req, State#{entity => Entity}}
  end.
