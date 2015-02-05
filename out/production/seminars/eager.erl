%%%-------------------------------------------------------------------
%%% @author Simon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. jan 2015 16:57
%%%-------------------------------------------------------------------
-module(eager).
-author("Simon").

%% API
-compile(export_all).

eval_expr({atm, Id}, _Env) ->                         % Atoms are literals
  {ok, Id};
eval_expr({var, Id}, Env) ->                          % Variables depend on the environment (env)
  case env:lookup(Id, Env) of                         % Check if variable is bound in env
    false ->                                          % If not bound, return an error
      error;
    {_Id, Str} ->                                     % If bound, return data structure variable is bound to
      {ok, Str}
  end;
eval_expr({cons, Head, Tail}, Env) ->                 % Cons are evaluated one element at a time
  case eval_expr(Head, Env) of
    error ->                                          % If first element is not in env, return error for the entire cons
      error;
    {ok, StrHead} ->                                  % If first element exists, check for second
      case eval_expr(Tail, Env) of
        error ->
          error;
        {ok, StrTail} ->                              % If both elements exist in env...
          {ok, {StrHead, StrTail}}                    % ... return tuple with both elements
      end
  end;
eval_expr({switch, Exp, Clauses}, Env) ->             % If the expression is a case expression...
  eval_case(Exp, Clauses, Env).                       % ...evaluate as case instead


eval_match(ignore, _Str, Env) ->                      % Don't care always exists
  {ok, Env};
eval_match({atm, Id}, Id, Env) ->                     % Atoms are literals
  {ok, Env};
eval_match({var, Id}, Str, Env) ->
  case env:lookup(Id, Env) of                         % Check for variable binding in env
    false ->                                          % If unbound...
      {ok, env:add(Id, Str, Env)};                    % ...add binding to new data structure
    {Id, Str} ->                                      % If bound to the structure that's sought for...
      {ok, Env};                                      % ...return the env with correct structure
    {Id, _} ->                                        % If it's bound to something else...
      fail                                            % ...return fail
  end;
eval_match({cons, Head, Tail}, {Str1, Str2}, Env) ->  % Cons are matched one element at a time to a corresponding data structure
  case eval_match(Head, Str1, Env) of
    fail ->                                           % No match returns fail
      fail;
    {ok, NewEnv} ->                                   % Matching succeded, returns a new env to evaluate next element in
      eval_match(Tail, Str2, NewEnv)                  % Evaluate match in new env
  end;
eval_match(_, _, _) ->                                % Anything that's not applicable returns fail
  fail.

eval_seq([Exp], Env) ->                               % A sequence that's just an expression is evaluated as expression
  eval_expr(Exp, Env);
eval_seq([{match, Ptr, Exp} | Seq], Env) ->           % Evaluate sequence one expression at a time
  case eval_expr(Exp, Env) of                         % Evaluate first expression (right-hand side)
    error ->
      error;
    {ok, Str} ->                                      % Evaluated expression returns data structure to match pattern to
      case eval_match(Ptr, Str, Env) of               % Attempt to match pattern with data structure
        fail ->
          error;
        {ok, NewEnv} ->                               % If successful, a new env is returned to keep evaluating sequence with
          eval_seq(Seq, NewEnv)
      end
  end.

eval(Seq) ->                                          % Help function to start with empty environment
  eval_seq(Seq, []).

eval_case(_Exp, [], _Env) ->                          % No more clauses and no matches is an error
  error;
eval_case(Exp, Clauses, Env) ->
  case eval_expr(Exp, Env) of                         % First evaluate expression to get data structure
    error ->                                          % Failed evaluation equals error
      error;
    {ok, Str} ->
      eval_clauses(Str, Clauses, Env)                 % Evaluate the clauses with the data structure
  end.

eval_clauses(Str, [{clause, Ptr, Seq} | Clauses], Env) ->
  case eval_match(Ptr, Str, Env) of                   % Pattern match first clause to data structure
    fail ->
      eval_clauses(Str, Clauses, Env);                % If fail, pattern match with pattern of next clause
    {ok, NewEnv} ->
      eval_seq(Seq, NewEnv)                           % If success, evaluate corresponding sequence to clause
  end.

% {var, x}, [{clause, {atm, b}, [{atm, c}]}], [{x, b}]
% case X of
% b -> c. (miljö x/b)

% {var, x}, [{clause, {atm, a}, [{atm, b}]}, {clause, {atm, c}, [{atm, d}]}], [{x, c}]
% case X of
% a -> b;
% c -> d. (miljö x/c)

% {var, x}, [{clause, {cons, {atm, a}, {atm, b}}, [{atm, foo}]}, {clause, {atm, b}, [{atm, y}]}, {clause, {cons, {atm, c}, ignore}, [{atm, bar}]}], [{x, {c, f}}]
% case X of
% {a, b} -> foo;
% b -> y;
% {c, _} -> bar. (miljö x/{c, f})

% {var, x}, [{clause, {atm, a}, [{switch, {var, y}, [{clause, {atm, b}, [{atm, c}]}]}]}], [{x, a}, {y, b}]
% case X of
% a ->
%     case Y of
%       b -> c.

