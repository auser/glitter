%%%-------------------------------------------------------------------
%%% File    : conf_writer.erl
%%% Author  : Ari Lerner
%%% Description :
%%%
%%% Created :  Fri Dec 18 23:09:34 PST 2009
%%%-------------------------------------------------------------------

-module (conf_writer).
-export ([write/2, write/3]).


write({config, Repos, Groups}, Loc) ->
  write(Repos, Groups, Loc);
write(Repos, Loc) ->
  Binary = format_repos(Repos),
  file:write_file(Loc, Binary).

write(Repos, Groups, Loc) ->
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
  UsersText = lists:map(fun({UName, Permission}) ->
                            "    " ++ Permission ++ " = " ++ UName ++ "\n"
                        end, Users),
  RepoText = lists:flatten([NameLine, UsersText, "\n"]),
  format_repos(Rest, [RepoText|List]).
