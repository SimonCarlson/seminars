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

new() ->                                              % Return an empty env
  [].

add(Id, Str, Env) ->                                  % Add a variable with it's structure to an env
  [{Id, Str} | Env].

lookup(_Id, []) ->                                    % Empty env -> no match
  false;
lookup(Id, [{Id, Str} | _T]) ->                       % Correct ID -> return structure
  {Id, Str};
lookup(Id, [{_,_} | T]) ->                            % Wrong ID -> recurse
  lookup(Id, T).


