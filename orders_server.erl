%%%-------------------------------------------------------------------
%%% @author raethlo
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Apr 2015 00:19
%%%-------------------------------------------------------------------
-module(orders_server).
-author("raethlo").

-behaviour(gen_server).

-record(order_state,{status,item}).

%% API
-export([start_link/0, status/1 ,cancel/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

status(Pid) ->
  gen_server:call(Pid,status).

cancel(Pid) ->
  gen_server:call(Pid,terminate).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #order_state{}} | {ok, State :: #order_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(Item) ->
  erlang:process_flag(trap_exit, true),
  io:format("New order created at (~w)~n",[self()]),
  {ok,#order_state{status=placed,item=Item}}.


handle_call(status, _From, State) ->
  {reply, State#order_state.status, State};
handle_call(terminate, _From, _State) ->
  {stop, normal, ok, _State}.


handle_cast(_Request,State) ->
  {noreply, State}.

handle_info(Msg, State) ->
  io:format("Order has got an unexpected message: ~p~n",[Msg]),
  {noreply, State}.

terminate(normal,State) ->
  io:format("Terminated the server ~n"),
  ok.

code_change(_OldSvn, State, _Extra) ->
  {ok, State}.

%% PRIVATE FUNCTIONS

