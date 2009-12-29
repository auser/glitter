%%%-------------------------------------------------------------------
%%% File    : conf_reader.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Fri Dec 18 19:12:44 PST 2009
%%%-------------------------------------------------------------------

-module (conf_reader).

-ifdef (debug).
-include_lib("eunit/include/eunit.hrl").
-endif.

-import(lists, [reverse/1]).
-export([parse_file/1,parse/1]).

-record (state, {
  state = none,         % gitosis_config | group_config | repos_config | other
  configs = []
}).

-record (section, {
  title = undefined,
  current_key = [],
  current_value,        % current parsing value
  current_values = [],
  proplist = []
}).

parse_file(FileName)  ->
  {ok, Binary} = file:read_file(FileName),
  parse(Binary).

parse(X) ->
  do_parse(X, #state{}).

%% whitespace, disregard
do_parse(<<32,Rest/binary>>, State) ->
  do_parse(Rest, State);
% This is a comment, let's skip to the next line
do_parse(<<$#,Rest/binary>>, State) ->
  do_parse(skip_to_new_line(Rest), State);
% Newline, skip!
do_parse(<<$\r,Rest/binary>>, State) -> do_parse(Rest, State);
do_parse(<<$\n,Rest/binary>>, State) -> do_parse(Rest, State);
% Starting to parse a config
do_parse(<<$[,Rest/binary>>, State) ->
  {NextSection, NewState} = parse_section(Rest, State),
  do_parse(NextSection, NewState);
do_parse(<<>>, #state{configs = Config} = _State) ->
  Config.

% We are in the section parsing now!
parse_section(<<>>, State) ->
  {<<>>, State};
parse_section(Rest, #state{configs = Config} = State) ->
  {Title, NextLine} = parse_section_title(Rest),
  {{SectionTitle, SectionProps} = Section, NextSection} = parse_config_section(NextLine, #section{title = Title}),
  
  NewState = case proplists:get_value(SectionTitle, Config) of
    undefined -> State#state{configs = lists:flatten([Section|Config])};
    Current -> State#state{configs = lists:flatten([SectionProps|Current])}
  end,
  parse_section(NextSection, NewState).
  
% Parse the config section
parse_config_section(<<$[,Rest/binary>>, #section{title = Title, proplist = Proplists} = _Section) -> 
  {{Title, Proplists}, Rest};
parse_config_section(<<>>, #section{title = Title, proplist = Proplists} = _Section) ->
  {{Title, Proplists}, <<>>};
parse_config_section(<<$=,Rest/binary>>, #section{current_key = CurrentKey, proplist = Proplists} = Section) ->
  CurrentKey1 = erlang:list_to_atom(chomp(CurrentKey)),
  {Values, NextLine} = parse_values(Rest, [], []),
  parse_config_section(NextLine, Section#section{
    current_key = [], 
    current_values = [], 
    current_value = [],
    proplist = [{CurrentKey1, Values}|Proplists]
    });
parse_config_section(<<$#,Rest/binary>>, State) -> parse_config_section(skip_to_new_line(Rest), State);

% If there is a newline, it could mean finish off the current key or it could be an erroneous newline
parse_config_section(<<$\n,Rest/binary>>, #section{current_key = []} = Section) -> parse_config_section(Rest, Section);
parse_config_section(<<$\n,Rest/binary>>, #section{current_key = K, current_values = Vals, proplist = Props} = Section) ->
  parse_config_section(Rest, Section#section{
    current_key = [], 
    current_values = [], 
    current_value = [], 
    proplist = [{K,Vals}|Props]
  });

parse_config_section(<<Chr,Rest/binary>>, #section{current_key = CurrentKey} = Section) ->
  parse_config_section(Rest, Section#section{current_key = [Chr|CurrentKey]});

parse_config_section(<<$\n,Rest/binary>>, #section{} = Section) -> parse_config_section(Rest, Section);
parse_config_section(<<$\r,Rest/binary>>, #section{} = Section) -> parse_config_section(Rest, Section).

% Parse values
% hello
% hello world
parse_values(<<$\n,Rest/binary>>, Acc, CurrVals) -> 
  Val = case CurrVals of
    [] -> [chomp(Acc)];
    _ -> [chomp(Acc)|CurrVals]
  end,
  {Val, Rest};
parse_values(<<32,Rest/binary>>, Acc, CurrVals) ->
  % Have we started, or is this erroneous whitespace
  case Acc of
    [] ->
      % erroneous
      parse_values(Rest, Acc, CurrVals);
    _ ->
      parse_values(Rest, [], [lists:reverse(Acc)|CurrVals])
  end;
parse_values(<<>>, Acc, CurrVals) ->
  Val = case CurrVals of
    [] -> [chomp(Acc)];
    _ -> [chomp(Acc)|CurrVals]
  end,
  {Val, <<>>};
parse_values(<<Chr,Rest/binary>>, Acc, CurrVals) ->
  parse_values(Rest, [Chr|Acc], CurrVals).
  
% skip_to_new_line
skip_to_new_line(Rest) -> skip_to_new_line1(Rest).
skip_to_new_line1(<<$\n,Rest/binary>>) ->  Rest;
skip_to_new_line1(<<$r,Rest/binary>>) ->  skip_to_new_line1(Rest);
skip_to_new_line1(<<>>) ->  <<>>;
skip_to_new_line1(<<_Ch,Rest/binary>>) -> skip_to_new_line1(Rest).

% Parse section titles
parse_section_title(Bin) -> parse_section_title(Bin, []).
parse_section_title(<<$],Rest/binary>>, Acc) -> {erlang:list_to_atom(lists:reverse(Acc)), Rest};
parse_section_title(<<Ch,Rest/binary>>, Acc) -> parse_section_title(Rest, [Ch|Acc]).

% Chomp the string for erroneous whitespace before and after
chomp(Str) -> 
  lists:reverse(string:strip(Str)).

-ifdef (debug).

chomp_test_() ->
  [
    ?_assertEqual(chomp(lists:reverse(" hello")), "hello"),
    ?_assertEqual(chomp(lists:reverse("hello ")), "hello"),
    ?_assertEqual(chomp(lists:reverse("hello world")), "hello world"),
    ?_assertEqual(lists:reverse(chomp(" hello   ")), "hello")
  ].
skip_to_new_line_test_() ->
  [
    ?_assertEqual(skip_to_new_line(<<"hello\nworld">>), <<"world">>),
    ?_assertEqual(skip_to_new_line(<<"hello\r world">>), <<>>),
    ?_assertEqual(skip_to_new_line(<<"hello world\ngoodnight moon">>), <<"goodnight moon">>)
  ].
parse_section_title_test_() ->
  [
    ?_assertEqual(parse_section_title(<<"box]">>), {box, <<>>}),
    ?_assertEqual(parse_section_title(<<"box]\n\n#hello world\n">>), {box, <<"\n\n#hello world\n">>})
  ].

full_parse_test_() ->
  Data = parse_file("env/gitosis.conf"),
  GitosisVal = proplists:get_value(gitosis, Data),
  AnotherGroup = proplists:get_value('group anothergroup', Data),
  TestRepos = proplists:get_value('group test_repos', Data),
  [
    ?_assertEqual([{members, ["alice", "bill"]}], AnotherGroup),
    ?_assertEqual([], TestRepos),
    ?_assertEqual([{gitweb, ["no"]}], GitosisVal)
  ].
  
-endif.