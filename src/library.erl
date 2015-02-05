%%%-------------------------------------------------------------------
%%% @author Simon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. jan 2015 14:01
%%%-------------------------------------------------------------------
-module(library).
-author("Simon").

%% API
-compile(export_all).

countTime({_, Sec2, MicroSec2}, {_, Sec1, MicroSec1}) ->
  Secs = Sec2 - Sec1,
  MicroSec = MicroSec2 - MicroSec1,
  Secs + 0.000001*MicroSec.

append([], B) ->
  B;
append([H | T], B) ->
  [H | append(T, B)].

reverse(List) ->
  reverse(List, []).
reverse(List, Rlist) ->
  case List of
    [] -> Rlist;
    [H | T] -> reverse(T, [H | Rlist])
  end.
