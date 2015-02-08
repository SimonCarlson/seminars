%%%-------------------------------------------------------------------
%%% @author Simon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. feb 2015 16:06
%%%-------------------------------------------------------------------
-module(philosopher).
-author("Simon").

%% API
-export([start/5]).

start(Hungry, Left, Right, Name, Ctrl) ->
  spawn_link(fun() -> sleeping(Hungry, Left, Right, Name, Ctrl) end).

sleeping(0, _Left, _Right, Name, Ctrl) ->
  io:format("~s is done!~n", [Name]),
  Ctrl ! done;
sleeping(Hungry, Left, Right, Name, Ctrl) ->
  Sleepytime = crypto:rand_uniform(0, 3000),
  io:format("~s is sleeping for ~w s ~n", [Name, Sleepytime / 1000]),
  sleep(Sleepytime),

  Left_Request = spawn_link(fun() -> chopstick:request(Left, self(), 2000) end),
  Right_Request = spawn_link(fun() -> chopstick:request(Right, self(), 2000) end),

  receive
    {ok,_} ->
      receive
        {no, Left_Request} ->
          sleeping(Hungry, Left, Right, Name, Ctrl);
        {no, Right_Request} ->
          chopstick:return(Left),
          sleeping(Hungry, Left, Right, Name, Ctrl);
        {ok, _} ->
          eat(),
          chopstick:return(Left),
          chopstick:return(Right),
          sleeping(Hungry - 1, Left, Right, Name, Ctrl)
      end;
    {no, _} ->
      na
  end.


eat() ->
  timer:sleep(1000).


sleep(D) ->
  timer:sleep(random:uniform(D)).


%BOMWOMBMBOOBMW
