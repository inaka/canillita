%%% @doc GET /news API handler
-module(canillita_news_handler).

-behaviour(trails_handler).
-behaviour(lasse_handler).

%% trails_handler behaviour callback
-export([trails/0]).

%% API
-export([notify/1]).

%% lasse_handler behaviour callbacks
-export([ init/3
        , handle_notify/2
        , handle_info/2
        , handle_error/3
        , terminate/3
        ]).

-type options() :: #{path => string()}.
-type last_event_id() :: binary() | undefined.
-type state() :: #{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% trails_handler callback
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec trails() -> trails:trails().
trails() ->
  Metadata =
    #{ get =>
       #{ tags => ["news"]
        , summary =>
          "WARNING: Do not try to use this endpoint from this page."
          " Swagger doesn't understand SSE"
        , description =>
          "Opens an [SSE] (http://www.w3.org/TR/eventsource/)"
          " connection to retrieve news updates"
        , externalDocs =>
          #{ description => "RFC"
           , url => "http://www.w3.org/TR/eventsource/"
           }
        , produces => ["application/json"]
        }
     },
  Path = "/news",
  Options = #{module => ?MODULE, init_args => #{path => Path}},
  [trails:trail(Path, lasse_handler, Options, Metadata)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc sends an event to all the listeners
-spec notify(Event::canillita_newsitems:news_item()) -> ok.
notify(Event) ->
  lists:foreach(
    fun(Listener) ->
      lasse_handler:notify(Listener, Event)
    end,
    pg2:get_members(canillita_listeners)
   ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% lasse_handler callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Will be called upon initialization of the handler, receiving the
%%      value of the "last-event-id" header if there is one or
%%      `undefined` otherwise.
-spec init( InitArgs::options()
          , LastEventId::last_event_id()
          , Req::cowboy_req:req()
          ) ->
  {ok, cowboy_req:req(), [lasse_handler:event()], state()}.
init(_InitArgs, LastEventId, Req) ->
  Req1 = sr_entities_handler:announce_req(Req, #{}),
  NewsItems = canillita_newsitems_repo:fetch_since(LastEventId),
  News = [canillita_newsitems:to_sse(NewsItem) || NewsItem <- NewsItems],
  ok = pg2:join(canillita_listeners, self()),
  {ok, Req1, News, #{}}.

%% @doc Receives and processes in-band messages sent through the
%%      lasse_handler:notify/2 function.
-spec handle_notify( NewsItem::canillita_newsitems:news_item()
                   , State::state()
                   ) -> {send, lasse_handler:event(), state()}.
handle_notify(NewsItem, State) ->
  Event = canillita_newsitems:to_sse(NewsItem),
  {send, Event, State}.

%% @doc Receives and processes out-of-band messages sent directly to the
%%      handler's process.
-spec handle_info(Info::any(), State::state()) -> lasse_handler:result().
handle_info(Info, State) ->
  error_logger:info_msg("~p received at ~p", [Info, State]),
  {nosend, State}.

%% @doc If there's a problem while sending a chunk to the client, this
%%      function will be called after which the handler will terminate.
-spec handle_error( Event::lasse_handler:event()
                  , Error::term()
                  , State::state()
                  ) -> state().
handle_error(Event, Error, State) ->
  error_logger:warning_msg(
    "Couldn't send ~p in ~p: ~p", [Event, State, Error]),
  State.

%% @doc This function will be called before terminating the handler, its
%%      return value is ignored.
-spec terminate(Reason::any(), Req::cowboy_req:req(), State::state()) -> ok.
terminate(Reason, _Req, _State) ->
  error_logger:info_msg("Terminating news: ~p", [Reason]),
  ok.
