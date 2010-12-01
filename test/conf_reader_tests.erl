-module(conf_reader_tests).

-include_lib("eunit/include/eunit.hrl").

full_parse_test() ->
  {state, Users, Groups} = conf_reader:parse_file("../test/example.conf"),
  [Arepo,Brepo] = Users,
  ?assertEqual({"arepo", [{"auser", "RW+"}]}, Arepo),
  ?assertEqual({"brepo", [{"jdunphy", "RW+"}, {"auser", "RW+"}]}, Brepo),
  ?assertEqual([{"@group", ["jdunphy","auser"]}], Groups),
  passed.

