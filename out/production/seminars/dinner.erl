%%%-------------------------------------------------------------------
%%% @author Simon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. feb 2015 16:02
%%%-------------------------------------------------------------------
-module(dinner).
-author("Simon").

%% API
-export([start/0]).

start() ->
  spawn(fun() -> init() end).

init() ->
  C1 = chopstick:start(),
  C2 = chopstick:start(),
  C3 = chopstick:start(),
  C4 = chopstick:start(),
  C5 = chopstick:start(),
  Ctrl = self(),
  philosopher:start(5, C1, C2, "1", Ctrl),

  philosopher:start(5, C2, C3, "2", Ctrl),

  philosopher:start(5, C3, C4, "3", Ctrl),

  philosopher:start(5, C4, C5, "4", Ctrl),

  philosopher:start(5, C5, C1, "5", Ctrl),
  wait(5, [C1, C2, C3, C4, C5]).

wait(0, Chopsticks) ->
  lists:foreach(fun(C) -> chopstick:quit(C) end, Chopsticks),
  io:format("Dinner is over!~n");
wait(N, Chopsticks) ->
  receive
    done ->
      wait(N - 1, Chopsticks);
    abort ->
      exit(abort)
  end.