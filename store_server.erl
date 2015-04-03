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

-include_lib("records.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, add_item/5 ,add_item/6, find/2, order/2,
        list/1, filter/2, close_shop/1, add_to_shopping_cart/4
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
  carts
}).


%%%===================================================================
%%% API
%%%===================================================================


add_item(Pid, Type, Name, Description, Price) when is_pid(Pid), is_atom(Type), is_number(Price)->
  add_item(Pid,Type, Name, Description, Price, 1).

add_item(Pid, Type, Name, Description, Price, Amount) when is_pid(Pid), is_atom(Type), is_number(Price),is_integer(Amount), Amount > 0 ->
  gen_server:call(Pid, {add_item, Type ,Name,Description,Price,Amount}).

add_to_shopping_cart(Pid, ClientPid, Item, Amount) when is_pid(Pid), is_pid(ClientPid), is_integer(Amount), Amount > 0->
  gen_server:call(Pid,{cart, ClientPid, Item, Amount}).

list(Pid) ->
  gen_server:call(Pid,list).

find(Pid, Name) ->
  gen_server:call(Pid, {find, Name}).

filter(Pid,Type) ->
  gen_server:call(Pid,{filter, Type}).

order(Pid,Name) ->
  gen_server:call(Pid, {order, Name}).

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
%%   erlang:process_flag(trap_exit, true),
  io:format("Store server succesefully started (~w)~n",[self()]),
  {ok, #state{
    items = maps:new(),
    carts = maps:new()
  }}.


handle_call({add_item,Type,Name,Description,Price,Amount},_From, State) ->
  NewItem = create_item(Type,Name,Description,Price),

  NewItems = update_items(NewItem,Amount,State#state.items),
  NewState = #state{items=NewItems,carts=State#state.carts},

  {reply, NewItem, NewState};

handle_call({cart, ClientPid, Item, Amount}, _From, State) ->
  OnStock = maps:get(Item,State#state.items,0) - Amount,
  if OnStock >= 0 ->
    NewItems = update_items(Item,-Amount,State#state.items),
    NewCarts = p_add_to_cart(ClientPid, Item, Amount, State#state.carts),
    {reply, ok, #state{items=NewItems,carts=NewCarts}};
  true ->
    {reply, {not_on_stock}, State}
  end;

handle_call({order,Name},_From, State) ->
%%   spawn order monitor and set the coresponding order

  {reply, result , State};

handle_call(list, _From, State) ->
%%   {reply, State#state.items, State};
  {reply, State, State};

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

create_item(Type,Name,Desc,Price) ->
  #item{type=Type ,name=Name,description=Desc,price=Price}.

update_items(Item,Amount,Items) ->
  CurrentAmount = maps:get(Item, Items, nil),
  case CurrentAmount of
    nil ->
      maps:put(Item, Amount, Items);
    N when is_integer(N), (CurrentAmount + Amount) > 0 ->
      maps:put(Item, CurrentAmount + Amount, Items);
    N when is_integer(N), (CurrentAmount + Amount) =< 0 ->
      maps:remove(Item, Items)
  end.

p_add_to_cart(UserPid, Item, Amount, Carts) when is_integer(Amount), Amount > 0 ->
  Cart = maps:get(UserPid, Carts, nil),
  if Cart =:= nil ->
      p_add_to_cart(UserPid,Item,Amount, maps:put(UserPid, maps:new(), Carts));
    true ->
      NewCart = update_items(Item, Amount, Cart),
      maps:put(UserPid,NewCart,Carts)
  end.

p_cart_empty(UserPid,Carts) ->
  maps:is_key(UserPid,Carts) andalso maps:get(UserPid,Carts) =/= maps:new().

p_discard_cart(UserPid) ->
  no.
