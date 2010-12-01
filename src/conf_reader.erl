%%%-------------------------------------------------------------------
%%% File    : conf_reader.erl
%%% Author  : Ari Lerner
%%% Description :
%%%
%%% Created :  Fri Dec 18 19:12:44 PST 2009
%%%-------------------------------------------------------------------

-module (conf_reader).

-export([parse_file/1,parse/1]).

-ifdef (TEST).
-compile(export_all).
-endif.

-record (state, {
           repos = []
          }).

-record (repo, {
           name = undefined,
           users = []
          }).

parse_file(FileName)  ->
  {ok, Binary} = file:read_file(FileName),
  parse(Binary).

parse(X) ->
  do_parse(X, #state{}).


do_parse(Bin, State) when is_binary(Bin) ->
  Chunks = re:split(Bin, "\\n"),
  do_parse(Chunks, State);
do_parse([], State) -> State;
do_parse([First|Lines], State) ->
  {Type, Data} = parse_line(First),
  case Type of
    repo ->
      {Users, NewLines} = parse_repo_users(Lines),
      Repo = {Data, Users},
      NewState = State#state{repos = State#state.repos ++ [Repo]},
      do_parse(NewLines, NewState);
    _ ->
      do_parse(Lines, State)
  end.

parse_line([]) -> {empty, []};
parse_line(<<$r,$e,$p,$o,Rest/binary>>) ->
  {repo, string:strip(binary_to_list(Rest))};
parse_line(<<$#,_/binary>>) ->
  {comment, undefined};
parse_line(Line) ->
  case re:run(Line, "=") of
    {match, _} ->
      [_Permission,Name] = re:split(Line, "="),
      {repo_user, string:strip(binary_to_list(Name))};
    nomatch -> {wtf, Line}
  end.


parse_repo_users(Lines) ->
  parse_repo_users(Lines, []).

parse_repo_users([<<$r,$e,$p,$o,_/binary>>|_] = Lines, Users) ->
  {Users, Lines};
parse_repo_users([], Users) ->
  {Users, []};
parse_repo_users([<<>>|Rest],Users) ->
  parse_repo_users(Rest, Users);
parse_repo_users([Line|Rest], Users) ->
  case(parse_line(Line)) of
    {repo_user, User} ->
      NewUsers = Users ++ [User],
      parse_repo_users(Rest, NewUsers);
    {_, _} -> parse_repo_users(Rest, Users)
  end.
