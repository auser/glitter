-module(conf_writer_tests).

-include_lib("eunit/include/eunit.hrl").

format_test_() ->
  [
   ?_assertEqual(["hello = world"],
                 conf_writer:format_value_proplists([{hello, ["world"]}])),
   ?_assertEqual(["so_long = thanks for all the fish"],
                 conf_writer:format_value_proplists([{so_long, ["thanks", "for", "all", "the", "fish"]}])),
   ?_assertEqual(["hello = world", "they = are here"],
                 conf_writer:format_value_proplists([{hello, ["world"]},
                                                     {they, ["are here"]}]))
  ].

format_section_test_() ->
  Props = [{'group anothergroup',[{members,["alice","bill"]}]}],
  [
   ?_assertEqual(["\n[group anothergroup]\nmembers = alice bill"],
                 conf_writer:format_section_proplists(Props))
  ].


