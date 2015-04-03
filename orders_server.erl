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

-include_lib("records.hrl").

-behaviour(gen_server).

-record(order_state,{customer_contact, status, item}).

%% API
-export([start_link/2, status/1 ,cancel/1, show/1]).

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

show(Pid) ->
  gen_server:call(Pid,show).

cancel(Pid) ->
  gen_server:call(Pid,terminate).

pay(Pid, Amount) when is_number(Amount), Amount > 0 ->
  gen_server:call(Pid,{pay,Amount}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

start_link(CustomerPid,Item) ->
  gen_server:start_link(?MODULE, {CustomerPid, Item}, []).

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
init({Contact,Item}) ->
  erlang:process_flag(trap_exit, true),
  io:format("New order created at (~w)~n",[self()]),
  {ok,#order_state{customer_contact = Contact, status=placed, item=Item}}.


handle_call(status, _From, State) ->
  {reply, State#order_state.status, State};

handle_call(show, _From, State) ->
  {reply, {State#order_state.status,State#order_state.item}, State};
%%   {reply, State, State};

handle_call(terminate, _From, _State) ->
  {stop, normal, ok, _State}.


handle_cast(_Request,State) ->
  {noreply, State}.

handle_info(Msg, State) ->
  io:format("Order has got an unexpected message: ~p~n",[Msg]),
  {noreply, State}.

terminate(normal,State) ->
  io:format("Order for ~s disappeared into the void ~n",[State#order_state.item#item.name]),
  ok.

code_change(_OldSvn, State, _Extra) ->
  {ok, State}.

%% PRIVATE FUNCTIONS

