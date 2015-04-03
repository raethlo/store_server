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

%% API
-export([]).


handle_info(Msg,State) ->
  io:format("Unexpected message for order ~s",[Msg]).