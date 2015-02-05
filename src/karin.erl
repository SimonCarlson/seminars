-module(karin).

-compile(export_all).

tree(Sample) ->
  Freq = freq(Sample),
  huffman(Freq).

freq(Sample) ->
  Base = base(),
  freq(Sample, Base).

freq([], Freq) ->
  Freq;
freq([Char|Rest], Freq) ->
  freq(Rest, update(Char, Freq)).

update(Char, []) ->
  [{Char, 1}];
update(Char, [{Char, N}|Freq]) ->
  [{Char, N+1}|Freq];
update(Char, [Elem|Freq]) ->
  [Elem|update(Char, Freq)].

huffman(Freq) ->
  Sorted = lists:sort(fun({_,X}, {_,Y}) -> X < Y end, Freq),
  huffman_tree(Sorted).

huffman_tree([{Tree, _}]) ->
  Tree;
huffman_tree([{A,Af}, {B,Bf} | Rest]) ->
  huffman_tree(insert({{A,B}, Af+Bf}, Rest)).

insert(A, []) ->
  [A];
insert({_,Af}=A, [{_,Bf}=B|Rest]) when Af < Bf ->
  [A, B | Rest];
insert(A, [B|Rest]) ->
  [B | insert(A, Rest)].

%%% Generating the table 

table(Tree) ->
  codes(Tree, []).


codes({A,B}, Sofar) ->
  As = codes(A, [0|Sofar]),
  Bs = codes(B, [1|Sofar]),
  As ++ Bs;
codes(A, Code) ->
  [{A, lists:reverse(Code)}].


%%& encoding the text using the table

encode([], _Table) ->
  [];
encode([C|Rest], Table) ->
  case code_lookup(C, Table) of
    {ok, Code} ->
      Code ++ encode(Rest, Table);
    false ->
      io:format("not found ~w ~n", [C]),
      encode(Rest, Table)
  end.

code_lookup(_, []) ->
  false;
code_lookup(Char, [{Char, Value} | _]) ->
  {ok, Value};
code_lookup(Char, [_ | Tail ]) ->
  code_lookup(Char, Tail).

%%% decoding the sequence using the tree

decode(Seq, Tree) ->
  na.

%% These are the characters that might be in the text

base() -> lists:map(fun(C) -> {C, 0} end,
  [10,
    32,33,34,            39,
    40,41,      44,45,46,   48,49,
    50,51,52,53,      56,57,58,59,
    60,61,62,63,   65,66,67,68,69,
    70,71,72,73,74,75,76,77,78,79,
    80,81,82,83,84,85,86,      89,
    97,98,99,
    100,101,102,103,104,105,106,107,108,109,
    110,111,112,    114,115,116,117,118,119,
    120,121,
    132,133,
    150,
    164,165,169,
    182,195,196,197,214,228,229,233,246]).


kallocain(N) ->
  {ok, Fd} = file:open("kallocain.txt", [read, binary]),
  {ok, Binary} = file:read(Fd, N),
  file:close(Fd),
  case unicode:characters_to_list(Binary, utf8) of
    {incomplete, List, _} ->
      List;
    List ->
      List
  end.

bench(N) ->
  Sample = kallocain(400),

  Text = kallocain(N),
  L = length(Text),

  Tree = tree(Sample),
  Table = table(Tree),

  S3 = now(),
  Seq = encode(Text, Table),
  T3 = timer:now_diff(now(), S3),

  E = length(Seq) div 8,
  io:format("encoded in ~w ms~n", [T3 div 1000]),
  io:format("source ~w bytes, endoded ~w bytes, ratio ~6.2f~n~n", [L, E, E/L]).

%  S4 = now(),
%  decode(Seq, Tree),
%  T4 = timer:now_diff(now(), S4),

%  io:format("decoded in ~w ms~n", [T4 div 1000]).


    
    
   
