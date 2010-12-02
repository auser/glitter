-module(conf_reader_tests).

-include_lib("eunit/include/eunit.hrl").

full_parse_test() ->
  {config, Repos, Groups} = conf_reader:parse_file("../test/example.conf"),
  [Arepo,Brepo] = Repos,
  ?assertEqual({"arepo", [{"auser", "RW+"}]}, Arepo),
  ?assertEqual({"brepo", [{"jdunphy", "RW+"}, {"auser", "RW+"}]}, Brepo),
  ?assertEqual([{"@group", ["jdunphy","auser"]}], Groups),
  passed.

parsing_multiple_repos_with_whitespace_test() ->
  Data = list_to_binary("\n  repo a\n    RW+ = u\n\n  repo b\n    RW+ = u\n\n"),
  {config, Repos, _G} = conf_reader:parse(Data),
  [{"a", _F}, {"b", _N}] =  Repos,
  passed.


