%%%-------------------------------------------------------------------
%%% @author Simon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. feb 2015 15:59
%%%-------------------------------------------------------------------
-module(chopstick).
-author("Simon").

%% API
-export([start/0, request/3, return/1, quit/1]).

start() ->
  spawn_link(fun() -> available() end).

available() ->
  receive
    {request,From} ->
      From ! granted,
      gone();
    quit ->
      ok
  end.

gone() ->
  receive
    return ->
      lib:flush_receive(),
      available();
    quit ->
      ok
  end.

request(Stick, From, Timeout) ->
  Stick ! {request, self()},
  receive
    granted ->
      From ! {ok, self()}
  after Timeout ->
    From ! {no, self()}
  end.

return(Stick) ->
  Stick ! return.

quit(Stick) ->
  Stick ! quit.