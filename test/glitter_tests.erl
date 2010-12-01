-module(glitter_tests).

-include_lib("eunit/include/eunit.hrl").

write_config_test_() ->
  {inorder,
   [
    fun setup/0,
    fun list_repos/0
   ]
  }.

setup() ->
  ok = application:set_env(glitter, config_file, "../test/example.conf"),
  glitter:start_link().

list_repos() ->
  [Arepo|_] = glitter:list_repos(),
  ?assertEqual({"arepo", [{"auser","RW+"}]}, Arepo),
  passed.
