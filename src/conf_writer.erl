%%%-------------------------------------------------------------------
%%% File    : conf_writer.erl
%%% Author  : Ari Lerner
%%% Description :
%%%
%%% Created :  Fri Dec 18 23:09:34 PST 2009
%%%-------------------------------------------------------------------

-module (conf_writer).

-export ([write/2]).

-ifdef(TEST).
-compile(export_all).
-endif.

write(Loc, Repos) ->
  Binary = format_repos(Repos),
  file:write_file(Loc, Binary).

write(Loc, Repos, Groups) ->
  {ok, F} = file:open(Loc, [write]),
  GroupsBin = format_groups(Groups),
  file:write(F, GroupsBin),
  ReposBin = format_repos(Repos),
  file:write(F, ReposBin),
  file:close(F),
  ok.

format_groups(Groups) ->
  format_groups(Groups, []).

format_groups([], List) ->
  list_to_binary(lists:flatten([List, "\n"]));
format_groups([Group|Rest], List) ->
  {Name, Users} = Group,
  UsersText = lists:map(fun(U) -> U ++ " " end, Users),
  GroupText = lists:flatten(["@", Name, " = ", UsersText, "\n"]),
  format_groups(Rest, [GroupText|List]).


format_repos(Repos) ->
   format_repos(Repos, []).

format_repos([], List) ->
  list_to_binary(lists:flatten(List));
format_repos([Repo|Rest], List) ->
  {Name, Users} = Repo,
  NameLine = "  repo " ++ Name ++ "\n",
  UsersText = lists:map(fun(U) -> U ++ " " end, Users),
  RepoText = lists:flatten([NameLine, "    RW+ = ", UsersText, "\n\n"]),
  format_repos(Rest, [RepoText|List]).
