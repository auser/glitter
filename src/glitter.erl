%%%-------------------------------------------------------------------
%%% File    : glitter.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sun Dec 20 13:29:52 PST 2009
%%%-------------------------------------------------------------------

-module (glitter).

-behaviour(gen_server).

%% API
-export ([
  list_repos/0,
  flush/0,
  has_git_repos/1,
  add_config/1,
  add_repos/1,
  remove_repos/1,
  add_user_to_repos/2,add_user_to_repos/3,
  remove_user_from_repos/2,remove_user_from_repos/3,
  add_user/2,
  reload/0,
  make_repos_public/1,
  make_repos_private/1,
  commit/0
]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
  gitosis_config,
  config
}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
list_repos() -> gen_server:call(?SERVER, {list_repos}).
has_git_repos(Name) -> gen_server:call(?SERVER, {has_git_repos, Name}).
add_config(Proplist) -> gen_server:call(?SERVER, {add_config, Proplist}).
add_repos(Name) -> gen_server:call(?SERVER, {add_repos, Name}).
remove_repos(Name) -> gen_server:call(?SERVER, {remove_repos, Name}).
add_user_to_repos(UserName, Name) -> gen_server:call(?SERVER, {add_user_to_repos, Name, UserName, members}).
add_user_to_repos(UserName, Name, Type) -> gen_server:call(?SERVER, {add_user_to_repos, Name, UserName, Type}).
remove_user_from_repos(UserName, Name) -> gen_server:call(?SERVER, {remove_user_from_repos, Name, UserName, members}).
remove_user_from_repos(UserName, Name, Type) -> gen_server:call(?SERVER, {remove_user_from_repos, Name, UserName, Type}).
add_user(UserName, Pubkey) -> gen_server:call(?SERVER, {add_new_user_and_key, UserName, Pubkey}).
make_repos_public(Name) -> gen_server:call(?SERVER, {make_repos_public, Name}).
make_repos_private(Name) -> gen_server:call(?SERVER, {make_repos_private, Name}).
flush() -> gen_server:cast(?SERVER, {flush}).
reload() -> gen_server:call(?SERVER, {reload}).
commit() -> gen_server:cast(?SERVER, {commit}).
  
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  {ok, ConfigFile} = application:get_env(glitter, config_file),
  Config = conf_reader:parse_file(ConfigFile),
  {ok, #state{
    gitosis_config = filename:absname(ConfigFile),
    config = Config
  }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({list_repos}, _From, #state{config = Config} = State) ->
  Reply = handle_list_repos(Config),
  {reply, Reply, State};
handle_call({add_config, Proplist}, _From, #state{config = Config} = State) ->
  NewState = handle_add_config(Proplist, Config, State),
  {reply, ok, NewState};
handle_call({add_repos, Name}, _From, #state{config = Config} = State) ->
  NewState = handle_add_repos(Name, Config, State),
  {reply, ok, NewState};
handle_call({remove_repos, Name}, _From, #state{config = Config} = State) ->
  NewState = handle_remove_repos(Name, Config, State),
  {reply, ok, NewState};
handle_call({add_user_to_repos, Name, UserName, Type}, _From, State) ->
  NewState = handle_add_user_to_repos(Name, UserName, Type, State),
  {reply, ok, NewState};
handle_call({remove_user_from_repos, Name, UserName, Type}, _From, State) ->
  NewState = handle_remove_user_from_repos(Name, UserName, Type, State),
  {reply, ok, NewState};
handle_call({add_new_user_and_key, UserName, Pubkey}, _From, State) ->
  NewState = handle_add_new_user_and_key(UserName, Pubkey, State),
  {reply, ok, NewState};
handle_call({make_repos_public, Name}, _From, State) ->
  NewState = handle_make_repos_public(Name, State),
  {reply, ok, NewState};
handle_call({make_repos_private, Name}, _From, State) ->
  NewState = handle_make_repos_private(Name, State),
  {reply, ok, NewState};
handle_call({has_git_repos, Name}, _From, #state{config = Config} = State) ->
  Reply = lists:member(Name, handle_list_repos(Config)),
  {reply, Reply, State};
handle_call({reload}, _From, #state{gitosis_config = ConfigFile} = State) ->
  Config = conf_reader:parse_file(ConfigFile),
  {reply, ok, State#state{config = Config}};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({flush}, State) ->
  flush(State),
  {noreply, State};
handle_cast({commit}, #state{gitosis_config = ConfigFile} = State) ->
  handle_commit(ConfigFile),
  flush(State),
  Config = conf_reader:parse_file(ConfigFile),
  {noreply, State#state{config = Config}};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
handle_add_config(Proplist, Config, State) ->
  NewConfig = case proplists:get_value(gitosis, Config) of
    undefined -> 
      [{gitosis, Proplist}|Config];
    GitosisConfig ->
      OldConfig = lists:delete(gitosis, Config),
      [{gitosis, lists:append([Proplist,GitosisConfig])}|OldConfig]
  end,
  NewState = State#state{config = NewConfig},
  flush(NewState),
  NewState.

handle_list_repos(Config) -> handle_list_repos(Config, []).
handle_list_repos([], Acc) -> lists:reverse(Acc);
handle_list_repos([{K, _V}|Rest], Acc) ->
  Key = erlang:atom_to_list(K),
  case string:substr(Key, 1, 6) =:= "group " of
    true -> 
      Name = string:substr(Key, 7, string:len(Key)),
      handle_list_repos(Rest, [Name|Acc]);
    false -> handle_list_repos(Rest, Acc)
  end.
  
handle_add_repos(undefined, _Config, State) -> State;
handle_add_repos(Name, Config, State) ->
  GroupName = erlang:list_to_atom(lists:append(["group ", Name])),
  NewConfig = case proplists:get_value(GroupName, Config) of
    undefined ->
      % New repos
      [{GroupName, [{writable, [Name]}]}|Config];
    _ -> 
      Config
  end,
  NewState = State#state{config = NewConfig},
  flush(NewState),
  NewState.

handle_remove_repos(undefined, _Config, _State) -> ok;
handle_remove_repos(Name, Config, State) ->
  GroupName = erlang:list_to_atom(lists:append(["group ", Name])),
  NewConfig = case proplists:is_defined(GroupName, Config) of
    true  -> proplists:delete(GroupName, Config);
    false -> Config
  end,
  NewState = State#state{config = NewConfig},
  flush(NewState),
  NewState.

handle_add_user_to_repos(Name, UserName, Type, #state{config = Config} = State) ->
  case find_already_defined_repos(Name, Config) of
    {error, not_found} -> 
      NewState = handle_add_repos(Name, Config, State),
      handle_add_user_to_repos(Name, UserName, Type, NewState);
    {ok, Key, Repos} ->
      NewReposConfig = case proplists:get_value(Type, Repos) of
        undefined -> [{Type, [UserName]}|Repos];
        CurrentMembers -> 
          case lists:member(UserName, CurrentMembers) of
            false ->
              OldRepos = proplists:delete(Type, Repos),
              [{Type, [UserName|CurrentMembers]}|OldRepos];
            _ -> Repos
          end
      end,
      OldConfig = proplists:delete(Key, Config),
      NewConfig = [{Key, NewReposConfig}|OldConfig],
      NewState = State#state{config = NewConfig},
      flush(NewState),
      NewState
  end.
  
handle_remove_user_from_repos(Name, UserName, Type, #state{config = Config} = State) ->
  case find_already_defined_repos(Name, Config) of
    {error, not_found} -> ok;
    {ok, Key, ReposConfig} ->
      NewReposConfig = case proplists:get_value(Type, ReposConfig) of
        undefined -> [{Type, [Name]}|ReposConfig];
        CurrentMembers -> 
          case lists:member(UserName, CurrentMembers) of
            true ->
              OldRepos = proplists:delete(Type, ReposConfig),
              NewMembers = lists:delete(UserName, CurrentMembers),
              [{Type, NewMembers}|OldRepos];
            _ -> ReposConfig
          end
      end,
      OldConfig = proplists:delete(Key, Config),
      NewConfig = [{Key, NewReposConfig}|OldConfig],
      NewState = State#state{config = NewConfig},
      flush(NewState),
      NewState
  end.

handle_add_new_user_and_key(UserName, Pubkey, #state{gitosis_config = ConfigFile} = State) ->
  case find_file_by_name(UserName, State) of
    {error, not_found} -> 
      Dirname = filename:dirname(ConfigFile),
      PubKeyfile = filename:join([Dirname, "keydir", lists:append(UserName, ".pub")]),
      {ok, Fd} = file:open(PubKeyfile, [write]),
      file:write(Fd, Pubkey),
      file:close(Fd);
    {ok, _Pubname}  -> ok
  end,
  State.
  
handle_make_repos_public(Name, State) ->
  handle_change_repos_config(Name, daemon, ["yes"], State).
handle_make_repos_private(Name, State) ->
  handle_change_repos_config(Name, daemon, ["no"], State).
    
handle_change_repos_config(RepoName, ConfigKey, ConfigVal, #state{config = Config} = State) ->
  case find_already_defined_repos(RepoName, Config) of
    {error, not_found} -> ok;
    {ok, Key, ReposConfig} ->
      NewReposConfig = case proplists:get_value(ConfigKey, ReposConfig) of
        undefined -> 
          [{ConfigKey, ConfigVal}|ReposConfig];
        Status ->
          case Status =:= ConfigVal of
            false ->
              OldRepos = proplists:delete(ConfigKey, ReposConfig),
              [{daemon, ConfigVal}|OldRepos];
            true -> ReposConfig
          end
      end,
      OldConfig = proplists:delete(Key, Config),
      NewConfig = [{Key, NewReposConfig}|OldConfig],
      NewState = State#state{config = NewConfig},
      flush(NewState),
      NewState
  end.
  
find_already_defined_repos(_Name, []) -> {error, not_found};
find_already_defined_repos(Name, [{K, V}|Rest]) ->
  Key = erlang:atom_to_list(K),
  case string:substr(Key, 1, 6) =:= "group " of
    true -> {ok, K, V};
    false -> find_already_defined_repos(Name, Rest)
  end.

flush(#state{config = Config, gitosis_config = ConfigFile} = _State) ->
  conf_writer:write(Config, ConfigFile).

find_file_by_name(Name, #state{gitosis_config = ConfigFile} = _State) ->
  {ok, Files} = file:list_dir(filename:dirname(ConfigFile)),
  find_file_by_name1(Name, Files).

find_file_by_name1(_Name, []) -> {error, not_found};
find_file_by_name1(Name, [K|Rest]) ->
  Pubname = lists:append([Name, ".pub"]),
  case K =:= Pubname of
    true -> {ok, Pubname};
    false -> find_file_by_name1(Name, Rest)
  end.

handle_commit(ConfigFile) ->
  Dirname = filename:dirname(ConfigFile),
  Command = lists:append(["cd ", Dirname, " && ", 
    "git add . && ",
    "git commit -a -m 'Updated from glitter'", " && ", "git push origin master"]),
  Out = os:cmd(Command),
  io:format("Out: ~p~n", [Out]),
  Out.