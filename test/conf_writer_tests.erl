-module(conf_writer_tests).

-include_lib("eunit/include/eunit.hrl").

write_config_test_() ->
  {inorder,
   [
    fun with_no_usergroup/0,
    fun with_usergroup/0
   ]
  }.

-define(WRITE_TEST, "../test/writetest.conf").

with_no_usergroup() ->
  clear_file(),
  Repos = [{"a_repo", [{"user_1", "RW+"}, {"user_2", "R"}]}],

  conf_writer:write(Repos, ?WRITE_TEST),
  {ok, File} = file:read_file(?WRITE_TEST),
  ?assertEqual("  repo a_repo\n    RW+ = user_1\n    R = user_2\n\n",
               binary_to_list(File)),
  passed.

with_usergroup() ->
  clear_file(),
  Repos = [{"a_repo", [{"@group", "RW+"}]}],
  UserGroups = [{"group", ["user_1", "user_2"]}],

  conf_writer:write(Repos, UserGroups, ?WRITE_TEST),

  {ok, File} = file:read_file(?WRITE_TEST),
  ?assertEqual("@group = user_1 user_2 \n\n  repo a_repo\n    RW+ = @group\n\n",
               binary_to_list(File)),
  passed.

clear_file() ->
  case(filelib:is_file(?WRITE_TEST)) of
    true -> file:delete(?WRITE_TEST);
    _ -> ok
  end.
