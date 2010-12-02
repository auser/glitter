%%%-------------------------------------------------------------------
%%% File    : conf_reader.erl
%%% Author  : Ari Lerner
%%% Description :
%%%
%%% Created :  Fri Dec 18 19:12:44 PST 2009
%%%-------------------------------------------------------------------

-module (conf_reader).

-export([parse_file/1,parse/1]).

-record (repo, {
           name = undefined,
           users = []
          }).

-include("glitter.hrl").

parse_file(FileName)  ->
  case file:read_file(FileName) of
    {ok, Binary} ->
      parse(Binary);
    {error, _} ->
      #config{}
  end.

parse(Binary) ->
  Chunks = re:split(Binary, "\\n"),
  do_parse(Chunks, #config{}).

do_parse([], Config) -> Config;
do_parse([First|Lines], Config) ->
  {Type, Data} = parse_line(First),
  case Type of
    repo ->
      {Users, NewLines} = parse_repo_users(Lines),
      Repo = {Data, Users},
      NewConfig = Config#config{repos = Config#config.repos ++ [Repo]},
      do_parse(NewLines, NewConfig);
    group ->
      NewConfig = Config#config{groups = Config#config.groups ++ [Data]},
      do_parse(Lines, NewConfig);
    _ ->
      do_parse(Lines, Config)
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

parse_repo_users([], Users) ->
  {Users, []};
parse_repo_users([<<>>|Rest],Users) ->
  parse_repo_users(Rest, Users);
parse_repo_users([Line|Rest] = Lines, Users) ->
  case(parse_line(Line)) of
    {repo, _} -> {Users, Lines};
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
