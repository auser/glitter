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

write(PropLists, To) ->
  Strings = format_section_proplists(lists:reverse(PropLists)),
  Str = string:join(Strings, "\n"),

  {ok, Fd} = file:open(To, [write]),
  file:write(Fd, Str),
  file:close(Fd),
  ok.

format_section_proplists(SectionProplists) ->
  format_section_proplists(SectionProplists, []).
format_section_proplists([], Acc) -> lists:reverse(Acc);
format_section_proplists([{SectionTitle, SectionProplists}|Rest], Acc) ->
  Title =
    lists:append(["\n", "[", erlang:atom_to_list(SectionTitle), "]", "\n"]),
  SectionStrings =
    lists:append([Title,
                  string:join(format_value_proplists(SectionProplists), "\n")]),
  format_section_proplists(Rest, [SectionStrings|Acc]).

format_value_proplists(PropLists) ->
  format_proplist(PropLists, []).
format_proplist([], Acc) -> lists:reverse(Acc);
format_proplist([{Title, Values}|Rest], Acc) ->
  case Values of
    [] -> format_proplist(Rest, Acc);
    _ ->
      Equals = lists:map(fun(Val) -> lists:append([" ", Val]) end, Values),
      Line = lists:flatten([erlang:atom_to_list(Title), " ", "=", Equals]),
      format_proplist(Rest, [Line|Acc])
  end.

