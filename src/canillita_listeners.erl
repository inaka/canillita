-module(canillita_listeners).

-behaviour(hive_session).

-type state() :: #{listener := pid()}.

-export([ broadcast/1
        , join/0
        ]).

-export([ init/2
        , handle_cast/2
        ]).

-spec broadcast(canillita_newsitems:news_item()) -> ok.
broadcast(NewsItem) -> hive:cast(canillita_listeners, NewsItem).

-spec join() -> {ok, hive:session_id()}.
join() -> hive:create(canillita_listeners, ?MODULE, self(), []).


-spec init(hive:id(), pid()) -> {ok, state()}.
init(_Id, Listener) -> {ok, #{listener => Listener}}.

-spec handle_cast(canillita_newsitems:news_item(), state()) ->
    {noreply, state()}
  | {stop, normal, state()}
  .
handle_cast(NewsItem, State) ->
  #{listener := Listener} = State,
  case rpc:pinfo(Listener) of
    undefined -> {stop, normal, State};
    _ ->
      ok = canillita_news_handler:notify(Listener, NewsItem),
      {noreply, State}
  end.
