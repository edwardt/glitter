%%%-------------------------------------------------------------------
%%% File    : conf_writer.erl
%%% Author  : 
%%% Description : configu reader for gitosis
%%%
%%% Created :  
%%%-------------------------------------------------------------------
-module(gitosis_conf_reader).
-export([main/1]).

file_to_stream(FileName) ->
  case file:read_file(FileName) of
    {ok, S} -> S;
    _ -> throw({error, could_not_read_file})
  end.

show_contents(<<>>, Acc) -> lists:reverse(Acc);
show_contents(<<$#,Rest/binary>>, Acc) -> show_contents(skip_rest_of_line(Rest), Acc);
show_contents(<<$\n,Rest/binary>>, Acc) -> show_contents(Rest, Acc);
show_contents(<<$\r,Rest/binary>>, Acc) -> show_contents(Rest, Acc);
show_contents(<<32,Rest/binary>>, Acc) -> show_contents(Rest, Acc);
show_contents(<<$r,$e,$p,$o,Rest/binary>>, Acc) -> 
  {RepoProplist, RestOfLine} = parse_repo_section(Rest, []),
  case proplists:get_value(repos, Acc) of
    undefined -> show_contents(RestOfLine, [{repos, RepoProplist}|Acc]);
    V -> 
      ProplistWithout = proplists:delete(repos, Acc),
      show_contents(RestOfLine, [{repos, [RepoProplist|V]}|ProplistWithout])
  end;
show_contents(<<_Chr, Rest/binary>>, Acc) -> show_contents(skip_rest_of_line(Rest), Acc).

skip_rest_of_line(<<$\n,Rest/binary>>) -> Rest;
skip_rest_of_line(<<>>) -> <<>>;
skip_rest_of_line(<<_Chr,Rest/binary>>) -> skip_rest_of_line(Rest).

parse_repo_section(<<32,Rest/binary>>, Acc) -> parse_repo_section(Rest, Acc);
parse_repo_section(<<$\t,Rest/binary>>, Acc) -> parse_repo_section(Rest, Acc);
parse_repo_section(<<$\n,Rest/binary>>, Acc) -> 
  {Proplist, RestOfLine} = parse_repo_config(Rest, []),
  SectionProps = {list_to_atom(lists:reverse(Acc)),Proplist},
  {SectionProps, RestOfLine};
parse_repo_section(<<Chr,Rest/binary>>, Acc) -> 
  parse_repo_section(Rest, [Chr|Acc]).

parse_repo_config(<<32,Rest/binary>>, Acc) -> parse_repo_config(Rest,Acc);
parse_repo_config(<<$\t,Rest/binary>>, Acc) -> parse_repo_config(Rest,Acc);
parse_repo_config(<<$\r,Rest/binary>>, Acc) -> parse_repo_config(Rest,Acc);
parse_repo_config(<<$#,Rest/binary>>, Acc) -> parse_repo_config(skip_rest_of_line(Rest), Acc);
parse_repo_config(<<$\n,Rest/binary>>, Acc) -> {Acc, Rest};
parse_repo_config(<<Chr, Rest/binary>>, Acc) -> 
  {KVPair, NextLine} = parse_repo_kv(Rest, key, [Chr], []),
  parse_repo_config(NextLine, [KVPair|Acc]).


parse_repo_kv(<<$=, Rest/binary>>, key, Key, Value) -> 
  parse_repo_kv(Rest, value, lists:reverse(Key), Value);
parse_repo_kv(<<Chr,Rest/binary>>, key, Key, Value) -> parse_repo_kv(Rest, key, [Chr|Key], Value); 
parse_repo_kv(<<$\n,Rest/binary>>, key, _Key, _Value) -> { {}, Rest };
parse_repo_kv(<<$\n,Rest/binary>>, value, Key, Value) -> { {Key, lists:reverse(Value)}, Rest };
parse_repo_kv(<<>>, value, Key, Value) -> { {Key, lists:reverse(Value)}, <<>> };
parse_repo_kv(<<Chr,Rest/binary>>, value, Key, Value) -> parse_repo_kv(Rest, value, Key, [Chr|Value]).


main(FileName) ->
  S = file_to_stream(FileName),
  show_contents(S,[]).
