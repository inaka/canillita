%%% @doc GET /newspapers/:name/news/:id handler.
-module(canillita_single_newsitem_handler).

-behaviour(trails_handler).

-include_lib("mixer/include/mixer.hrl").
-mixin([{ sr_single_entity_handler
        , [ init/3
          , rest_init/2
          , allowed_methods/2
          , content_types_accepted/2
          , content_types_provided/2
          , handle_get/2
          ]
        }]).
-mixin([{ sr_entities_handler, [announce_req/2] }]).

%% Alias
-type state() :: sr_single_entity_handler:state().

-export([ trails/0, resource_exists/2 ]).

-spec trails() -> trails:trails().
trails() ->
  NewspaperName =
    #{ name => name
     , in => path
     , description => <<"Newspaper name">>
     , required => true
     , type => string
     },
  NewsItemId =
    #{ name => id
     , in => path
     , descripcion => <<"News item id">>
     , required => true
     , type => string
     },
  Metadata =
    #{ get =>
       #{ tags => ["newsitems"]
        , description => "Return a newsitem"
        , produces => ["application/json"]
        , parameters => [NewspaperName, NewsItemId]
        }
     },
  Path = "/newspapers/:name/news/:id",
  Options = #{path => Path, model => canillita_newsitems},
  [trails:trail(Path, ?MODULE, Options, Metadata)].

-spec resource_exists( Req::cowboy_req:req(), State::state()) ->
  {boolean(), cowboy_req:req(), state()}.
resource_exists(Req, State) ->
  #{opts := #{model := Model}, id := Id} = State,
  {NewspaperName, Req2} = cowboy_req:binding(name, Req),
  Conditions = [{id, Id}, {newspaper_name, NewspaperName}],
  case canillita_newsitems_repo:newsitem_exists(Model, Conditions) of
    notfound -> {false, Req2, State};
    Entity -> {true, Req2, State#{entity => Entity}}
  end.
