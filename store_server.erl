%%%-------------------------------------------------------------------
%%% @author raethlo
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2015 13:20
%%%-------------------------------------------------------------------
-module(store_server).
-author("raethlo").

-behaviour(gen_server).

%% API
-export([start_link/0, add_item/4, find/2, order/2, pay/0,
        list/1, close_shop/1
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  items,
  orders
}).
-record(item,{name, type, description="No dedcription provided.", price}).

%%%===================================================================
%%% API
%%%===================================================================

add_item(Pid, Name, Description, Price) ->
  gen_server:call(Pid, {add_item,Name,Description,Price}).

list(Pid) ->
  gen_server:call(Pid,list).

find(Pid, Name) ->
  gen_server:call(Pid, {find, Name}).

order(Pid,Name) ->
  gen_server:call(Pid, {order, Name}).

pay() ->
%%
  not_yet.

close_shop(Pid) ->
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
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  io:format("Store server succesefully started (~w)~n",[self()]),
  {ok, #state{
    items = maps:new()
  }}.


handle_call({add_item,Type,Name,Description,Price},_From, State) ->
  NewItem = create_item(Name,Description,Price),
  NewState = #state{items=maps:put(Name,NewItem,State#state.items)},
  {reply, NewItem, NewState};

handle_call({order,Name},_From, State) ->
%%   spawn order monitor and set the coresponding order
  {reply, not_implemented, State};

handle_call(list, _From, State) ->
  {reply, maps:values(State#state.items), State};

handle_call(terminate, _From, _State) ->
  {stop, normal, ok, _State}.


handle_cast(_Request,State) ->
  {noreply, State}.

handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, State}.

terminate(normal,State) ->
  io:format("Terminated the server ~n"),
  ok.

code_change(_OldSvn, State, _Extra) ->
  {ok, State}.

%% PRIVATE FUNCTIONS

create_item(Name,Desc,Price) ->
  #item{name=Name,description=Desc,price=Price}.