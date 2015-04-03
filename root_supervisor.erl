%%%-------------------------------------------------------------------
%%% @author raethlo
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2015 23:49
%%%-------------------------------------------------------------------
-module(root_supervisor).
-author("raethlo").

-behaviour(supervisor).

%% API
-export([start_link/0, shutdown/0 ]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

shutdown() ->
  exit(normal).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 60,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = infinity,
  Type = supervisor,

  StoreSupervisor = {store_supervisor, {store_supervisor, start_link, []},
    Restart, Shutdown, Type, [store_supervisor]},

  OrdersSupervisor = {orders_supervisor, {orders_supervisor, start_link, []},
    Restart, Shutdown, Type, [orders_supervisor]},

  io:format("Root supervisor ready and all alive at (~w)~n",[self()]),
  {ok, {SupFlags, [StoreSupervisor, OrdersSupervisor]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
