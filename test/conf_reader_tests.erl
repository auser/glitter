-module(conf_reader_tests).

-include_lib("eunit/include/eunit.hrl").

full_parse_test() ->
  {state, Data} = conf_reader:parse_file("../test/example.conf"),
  [Arepo,Brepo] = Data,
  ?assertEqual(Arepo, {"arepo", ["auser"]}),
  ?assertEqual(Brepo, {"brepo", ["jdunphy", "auser"]}).

