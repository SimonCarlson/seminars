-module(morse).

-compile(export_all).


zero() ->
  ".- .-.. .-.. ..-- -.-- --- ..- .-. ..-- -... .- ... . ... ..-- .- .-. . ..-- -... . .-.. --- -. --. ..-- - --- ..-- ..- ... ".

rolled() ->
  ".... - - .--. ... ---... .----- .----- .-- .-- .-- .-.-.- -.-- --- ..- - ..- -... . .-.-.- -.-. --- -- .----- .-- .- - -.-. .... ..--.. ...- .----. -.. .--.-- ..... .---- .-- ....- .-- ----. .--.-- ..... --... --. .--.-- ..... ---.. -.-. .--.-- ..... .---- ".

%% {node, Char, Long, Short} empty branches nil

decode_table() ->
  {node,na,
    {node,116,
      {node,109,
        {node,111,
          {node,na,{node,48,nil,nil},{node,57,nil,nil}},
          {node,na,nil,{node,56,nil,{node,58,nil,nil}}}},
        {node,103,
          {node,113,nil,nil},
          {node,122,
            {node,na,{node,44,nil,nil},nil},
            {node,55,nil,nil}}}},
      {node,110,
        {node,107,{node,121,nil,nil},{node,99,nil,nil}},
        {node,100,
          {node,120,nil,nil},
          {node,98,nil,{node,54,{node,45,nil,nil},nil}}}}},
    {node,101,
      {node,97,
        {node,119,
          {node,106,
            {node,49,{node,47,nil,nil},{node,61,nil,nil}},
            nil},
          {node,112,
            {node,na,{node,37,nil,nil},{node,64,nil,nil}},
            nil}},
        {node,114,
          {node,na,nil,{node,na,{node,46,nil,nil},nil}},
          {node,108,nil,nil}}},
      {node,105,
        {node,117,
          {node,32,
            {node,50,nil,nil},
            {node,na,nil,{node,63,nil,nil}}},
          {node,102,nil,nil}},
        {node,115,
          {node,118,{node,51,nil,nil},nil},
          {node,104,{node,52,nil,nil},{node,53,nil,nil}}}}}}.


%% ordered tree {node, Char, Code, Low, High} empty branches nil


encode_table() ->
  {node,100,"-..",
    {node,53,".....",
      {node,47,".-----",
        {node,44,"--..--",
          {node,32,"..--",nil,{node,37,".--.--",nil,nil}},
          {node,45,"-....-",nil,{node,46,".-.-.-",nil,nil}}},
        {node,50,"..---",
          {node,48,"-----",nil,{node,49,".----",nil,nil}},
          {node,51,"...--",nil,{node,52,"....-",nil,nil}}}},
      {node,61,".----.",
        {node,56,"---..",
          {node,54,"-....",nil,{node,55,"--...",nil,nil}},
          {node,57,"----.",nil,{node,58,"---...",nil,nil}}},
        {node,97,".-",
          {node,63,"..--..",nil,{node,64,".--.-.",nil,nil}},
          {node,98,"-...",nil,{node,99,"-.-.",nil,nil}}}}},
    {node,112,".--.",
      {node,106,".---",
        {node,103,"--.",
          {node,101,".",nil,{node,102,"..-.",nil,nil}},
          {node,104,"....",nil,{node,105,"..",nil,nil}}},
        {node,109,"--",
          {node,107,"-.-",nil,{node,108,".-..",nil,nil}},
          {node,110,"-.",nil,{node,111,"---",nil,nil}}}},
      {node,118,"...-",
        {node,115,"...",
          {node,113,"--.-",nil,{node,114,".-.",nil,nil}},
          {node,116,"-",nil,{node,117,"..-",nil,nil}}},
        {node,121,"-.--",
          {node,119,".--",nil,{node,120,"-..-",nil,nil}},
          {node,122,"--..",nil,nil}}}}}.


decode(Text) ->
  decode(Text, decode_table(), decode_table(), []).

decode([], _, _, Decoded) ->
  lists:reverse(Decoded);
decode([$- | T], {_, _, LeftNode, _}, Table, Decoded) ->
  decode(T, LeftNode, Table, Decoded);
decode([$. | T], {_, _, _, RightNode}, Table, Decoded) ->
  decode(T, RightNode, Table, Decoded);
decode([$  | T], {_, Value, _, _}, Table, Decoded) ->
  decode(T, Table, Table, [Value | Decoded]).