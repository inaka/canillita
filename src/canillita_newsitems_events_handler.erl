-module(canillita_newsitems_events_handler).

-behaviour(gen_event).

-export([ init/1
        , terminate/2
        , handle_info/2
        , handle_call/2
        , code_change/3
        , handle_event/2
        ]).

-type state() :: [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_event functions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init([]) -> {ok, state()}.
init([]) ->
  {ok, []}.

-spec handle_info(_, state()) -> {ok, state()}.
handle_info(_Info, State) ->
  {ok, State}.

-spec handle_call(_, state()) -> {ok, not_implemented, state()}.
handle_call(_Request, State) ->
  {ok, not_implemented, State}.

-spec handle_event(term(), state()) -> {ok, state()}.
handle_event({_EventId, canillita_newsitems, persisted, [NewsItem]}, State) ->
  canillita_listeners:broadcast(NewsItem),
  {ok, State};
handle_event(Event, State) ->
  _ = lager:info("Ignored event: ~p", [Event]),
  {ok, State}.

-spec code_change(_, state(), _) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-spec terminate(_, _) -> ok.
terminate(_Arg, _State) ->
  ok.
