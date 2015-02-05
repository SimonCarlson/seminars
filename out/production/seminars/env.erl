%%%-------------------------------------------------------------------
%%% @author Simon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. jan 2015 16:34
%%%-------------------------------------------------------------------
-module(env).
-author("Simon").

%% API
-compile(export_all).

new() ->
  [].

add(Id, Str, Env) ->
  [{Id, Str} | Env].

lookup(_Id, []) ->
  false;
lookup(Id, [{Id, Str} | _T]) ->
  {Id, Str};
lookup(Id, [{_,_} | T]) ->
  lookup(Id, T).


