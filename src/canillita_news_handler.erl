%% @doc Handler for /news endpoints
-module(canillita_news_handler).
-author('elbrujohalcon@inaka.net').

-export(
  [ init/3
  , allowed_methods/2
  , content_types_accepted/2
  , resource_exists/2
  , info/3
  , terminate/3
  ]).

-export(
  [ handle_post/2
  , handle_get/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% COWBOY CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Transport, Req, _Opts) ->
  case cowboy_req:method(Req) of
    {<<"POST">>, _} ->
      {upgrade, protocol, cowboy_rest};
    {<<"GET">>, Req1} ->
      handle_get(Req1)
  end.

allowed_methods(Req, State) -> {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  {[{<<"application/json">>, handle_post}], Req, State}.

resource_exists(Req, State) -> {false, Req, State}.

info({news_flash, NewsFlash}, Req, State) ->
  send_flash(NewsFlash, Req),
  {loop, Req, State, hibernate}.

terminate(_Reason, _Req, _State) -> ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_post(Req, State) ->
  {ok, Body, Req1} = cowboy_req:body(Req),

  case json_decode(Body) of
    {Params} ->
      Title   = proplists:get_value(<<"title">>,    Params, <<"Generic News">>),
      Content = proplists:get_value(<<"content">>,  Params, <<"">>),
      
      NewsFlash = canillita_news:new(Title, Content),
      notify(NewsFlash),

      {true, Req1, State};
    {bad_json, Reason} ->
      {ok, Req2} = cowboy_req:reply(400, [], jiffy:encode(Reason), Req1),
      {halt, Req2, State}
  end.

handle_get(Req) ->
  {ok, Req1} =
    cowboy_req:chunked_reply(
      200, [{"content-type", <<"text/event-stream">>}], Req),

  LatestNews = canillita_news:latest_news(),

  lists:foreach(
    fun(NewsFlash) ->
      send_flash(NewsFlash, Req1)
    end, LatestNews),

  ok = pg2:join(canillita_listeners, self()),

  {loop, Req1, undefined, hibernate}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% AUXILIARY FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
json_decode(Bin) ->
  try jiffy:decode(Bin)
  catch
      _:{error, {Char, invalid_json}} ->
          {bad_json, {[{invalid_json, Bin}, {character, Char}]}};
      _:{error, {Char, truncated_json}} ->
          {bad_json, {[{truncated_json, Bin}, {character, Char}]}};
      _:{error, {Char, invalid_trailing_data}} ->
          {bad_json, {[{invalid_trailing_data, Bin}, {character, Char}]}};
      _:Error ->
          {bad_json, iolist_to_binary(io_lib:format("~p", [Error]))}
  end.

notify(NewsFlash) ->
  lists:foreach(
    fun(Listener) ->
      Listener ! {news_flash, NewsFlash}
    end, pg2:get_members(canillita_listeners)).

send_flash(NewsFlash, Req) ->
  Event = canillita_news:get_title(NewsFlash),
  Data  = canillita_news:get_content(NewsFlash),
  send_data(Event, Data, Req).

send_data(Event, Data, Req) ->
  Chunk = <<"event: ", Event/binary, "\ndata: ", Data/binary, "\n\n">>,
  cowboy_req:chunk(Chunk, Req).
