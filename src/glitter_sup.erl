%%%-------------------------------------------------------------------
%%% File    : glitter_sup.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Sun Dec 20 13:29:41 PST 2009
%%%-------------------------------------------------------------------

-module (glitter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
  AppSrv = {the_glitter_srv,{glitter_srv, start_link,[]}, permanent,2000,worker,dynamic},
  
  {ok,{{one_for_one,5,10}, [AppSrv]}}.

%%====================================================================
%% Internal functions
%%====================================================================