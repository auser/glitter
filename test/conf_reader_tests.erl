-module(conf_reader_tests).

-include_lib("eunit/include/eunit.hrl").

chomp_test_() ->
  [
    ?_assertEqual(conf_reader:chomp(lists:reverse(" hello")), "hello"),
    ?_assertEqual(conf_reader:chomp(lists:reverse("hello ")), "hello"),
    ?_assertEqual(conf_reader:chomp(lists:reverse("hello world")), "hello world"),
    ?_assertEqual(lists:reverse(conf_reader:chomp(" hello   ")), "hello")
  ].
skip_to_new_line_test_() ->
  [
    ?_assertEqual(conf_reader:skip_to_new_line(<<"hello\nworld">>), <<"world">>),
    ?_assertEqual(conf_reader:skip_to_new_line(<<"hello\r world">>), <<>>),
    ?_assertEqual(conf_reader:skip_to_new_line(<<"hello world\ngoodnight moon">>),
                  <<"goodnight moon">>)
  ].
parse_section_title_test_() ->
  [
    ?_assertEqual(conf_reader:parse_section_title(<<"box]">>), {box, <<>>}),
    ?_assertEqual(conf_reader:parse_section_title(<<"box]\n\n#hello world\n">>),
                  {box, <<"\n\n#hello world\n">>})
  ].


full_parse_test() ->
  erlang:display(filelib:is_file("../test/example.conf")),
  Data = conf_reader:parse_file("../test/example.conf").

