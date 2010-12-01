%%%-------------------------------------------------------------------
%%% File    : conf_reader.erl
%%% Author  : Ari Lerner
%%% Description :
%%%
%%% Created :  Fri Dec 18 19:12:44 PST 2009
%%%-------------------------------------------------------------------

-module (conf_reader).

-export([parse_file/1,parse/1]).

-record (state, {
           repos = [],
           groups = []
          }).

-record (repo, {
           name = undefined,
           users = []
          }).

parse_file(FileName)  ->
  {ok, Binary} = file:read_file(FileName),
  parse(Binary).

parse(Binary) ->
  Chunks = re:split(Binary, "\\n"),
  do_parse(Chunks, #state{}).

do_parse([], State) -> State;
do_parse([First|Lines], State) ->
  {Type, Data} = parse_line(First),
  case Type of
    repo ->
      {Users, NewLines} = parse_repo_users(Lines),
      Repo = {Data, Users},
      NewState = State#state{repos = State#state.repos ++ [Repo]},
      do_parse(NewLines, NewState);
    group ->
      NewState = State#state{groups = State#state.groups ++ [Data]},
      do_parse(Lines, NewState);
    _ ->
      do_parse(Lines, State)
  end.

parse_line([]) -> {empty, []};
parse_line(<<$r,$e,$p,$o,Rest/binary>>) ->
  {repo, string:strip(binary_to_list(Rest))};
parse_line(<<$#,_/binary>>) ->
  {comment, undefined};
parse_line(<<$\ ,Rest/binary>>) ->
  parse_line(Rest);
parse_line(<<$@,_/binary>> = Binary) ->
  [Name, UserChunk] = re:split(string:strip(binary_to_list(Binary)), "="),
  Users = re:split(UserChunk, "\ "),
  UserList = list_users(Users),
  {group, {string:strip(binary_to_list(Name)), UserList}};
parse_line(Binary) ->
  case re:run(Binary, "=") of
    {match, _} ->
      [Permission,Name] = re:split(Binary, "="),
      {repo_user, {string:strip(binary_to_list(Name)),
                   string:strip(binary_to_list(Permission))}};
    nomatch -> {nomatch, Binary}
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

%% Expecting a list of binaries
list_users([]) -> [];
list_users([<<>>|Users]) ->
  list_users(Users);
list_users([First|Users]) ->
  [string:strip(binary_to_list(First))|list_users(Users)].
