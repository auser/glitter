-module(glitter_tests).

-include_lib("eunit/include/eunit.hrl").

write_config_test_() ->
  {inorder,
   [
    fun list_repos/0
   ]
  }.

list_repos() ->
  passed.
