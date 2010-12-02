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
          add_repos/1,
          remove_repos/1,
          add_user_to_repos/2,
          remove_user_from_repos/2,
          add_user/2,
          reload/0,
          commit/0,
          stop/0
         ]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("glitter.hrl").

-record(state, {
          gitolite_config,
          config
         }).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
list_repos() -> gen_server:call(?SERVER, {list_repos}).
has_git_repos(Name) -> gen_server:call(?SERVER, {has_git_repos, Name}).
add_repos(Name) -> gen_server:call(?SERVER, {add_repos, Name}).
remove_repos(Name) -> gen_server:call(?SERVER, {remove_repos, Name}).
add_user_to_repos(UserInfo, Name) ->
  gen_server:call(?SERVER, {add_user_to_repos, Name, UserInfo}).
remove_user_from_repos(UserName, Name) ->
  gen_server:call(?SERVER, {remove_user_from_repos, Name, UserName}).
add_user(UserInfo, Pubkey) ->
  gen_server:call(?SERVER, {add_new_user_and_key, UserInfo, Pubkey}).
flush() -> gen_server:cast(?SERVER, {flush}).
reload() -> gen_server:call(?SERVER, {reload}).
commit() -> gen_server:cast(?SERVER, {commit}).

stop() ->
  gen_server:cast(?SERVER, {stop}).

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
     gitolite_config = filename:absname(ConfigFile),
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
handle_call({add_repos, Name}, _From, #state{config = Config} = State) ->
  NewState = handle_add_repos(Name, Config, State),
  {reply, ok, NewState};
handle_call({remove_repos, Name}, _From, #state{config = Config} = State) ->
  NewState = handle_remove_repos(Name, Config, State),
  {reply, ok, NewState};
handle_call({add_user_to_repos, Name, UserInfo}, _From, State) ->
  NewState = handle_add_user_to_repos(Name, UserInfo, State),
  {reply, ok, NewState};
handle_call({remove_user_from_repos, Name, UserName}, _From, State) ->
  NewState = handle_remove_user_from_repos(Name, UserName, State),
  {reply, ok, NewState};
handle_call({add_new_user_and_key, UserInfo, Pubkey}, _From, State) ->
  NewState = handle_add_new_user_and_key(UserInfo, Pubkey, State),
  {reply, ok, NewState};
handle_call({has_git_repos, Name}, _From, #state{config = Config} = State) ->
  Reply = lists:any(fun(R) -> Name =:= element(1,R) end,
                    handle_list_repos(Config)),
  {reply, Reply, State};
handle_call({reload}, _From, #state{gitolite_config = ConfigFile} = State) ->
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
handle_cast({stop}, State) ->
  {stop, normal, State};
handle_cast({flush}, State) ->
  flush(State),
  {noreply, State};
handle_cast({commit}, #state{gitolite_config = ConfigFile} = State) ->
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

handle_list_repos(Config) ->
  Config#config.repos.

handle_add_repos(undefined, _Config, State) -> State;
handle_add_repos(Name, Config, State) ->
  NewConfig =
    case lists:any(fun(Repo) ->
                       Name =:= element(1,Repo)
                   end, Config#config.repos) of
      false ->
        %% New config
        Config#config{repos = [{Name, []}|Config#config.repos]};
      _ ->
        Config
    end,
  NewState = State#state{config = NewConfig},
  flush(NewState),
  NewState.

handle_remove_repos(undefined, _Config, _State) -> ok;
handle_remove_repos(Name, Config, State) ->
  Repos = Config#config.repos,
  NewConfig = case proplists:is_defined(Name, Repos) of
                true  ->
                  NewRepos = proplists:delete(Name, Repos),
                  Config#config{repos = NewRepos};
                false -> Config
              end,
  NewState = State#state{config = NewConfig},
  flush(NewState),
  NewState.


%% UserInfo -> {"name", "permission"}
handle_add_user_to_repos(Name, UserInfo, #state{config = Config} = State) ->
  case find_already_defined_repos(Name, Config) of
    {error, not_found} ->
      NewState = handle_add_repos(Name, Config, State),
      handle_add_user_to_repos(Name, UserInfo, NewState);
    {ok, Users} ->
      NewReposConfig =
        case proplists:is_defined(element(1,UserInfo), Users) of
          false -> [UserInfo|Users];
          true ->
            OldUsers = proplists:delete(element(1,UserInfo), Users),
            [UserInfo|OldUsers]
        end,
      OldRepos = proplists:delete(Name, Config#config.repos),
      NewRepos = [{Name, NewReposConfig}|OldRepos],
      NewState = State#state{config = Config#config{repos = NewRepos}},
      flush(NewState),
      NewState
  end.

handle_remove_user_from_repos(Name, UserName,
                              #state{config = Config} = State) ->
  case find_already_defined_repos(Name, Config) of
    {error, not_found} -> ok;
    {ok, Users} ->
      case proplists:is_defined(UserName, Users) of
        false -> ok;
        true ->
          OtherUsers = proplists:delete(UserName, Users),
          UpdatedRepo = {Name, OtherUsers},
          OtherRepos = proplists:delete(Name, Config#config.repos),
          NewConfig = Config#config{repos = [UpdatedRepo|OtherRepos]},
          NewState = State#state{config = NewConfig},
          flush(NewState),
          NewState
      end
  end.

handle_add_new_user_and_key(UserInfo, Pubkey,
                            #state{gitolite_config = ConfigFile} = State) ->
  case find_file_by_name(UserInfo, State) of
    {error, not_found} ->
      Dirname = filename:dirname(ConfigFile),
      PubKeyfile =
        filename:join([Dirname, "keydir", lists:append(UserInfo, ".pub")]),
      {ok, Fd} = file:open(PubKeyfile, [write]),
      file:write(Fd, Pubkey),
      file:close(Fd);
    {ok, _Pubname}  -> ok
  end,
  State.


find_already_defined_repos(Name, Config) when is_record(Config, config) ->
  find_already_defined_repos(Name, Config#config.repos);
find_already_defined_repos(Name, Repos) ->
  case proplists:get_value(Name, Repos) of
    undefined -> {error, not_found};
    Result -> {ok, Result}
  end.

flush(#state{config = Config, gitolite_config = ConfigFile} = _State) ->
  conf_writer:write(Config, ConfigFile).

find_file_by_name(Name, #state{gitolite_config = ConfigFile} = _State) ->
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
