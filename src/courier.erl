%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(courier).
-author("ryandenby").

%% API
-export([start/3]).

-type resource()  :: {URI :: string(),
                      Handler :: module(),
                      HandlerArgs :: [term()]}.
-type resources() :: [resource()].

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

-spec start(PoolRef :: atom(), PoolOpts :: courier_pool:pool_opts(),
            Resources :: resources()) ->
        ok.
start(PoolRef, PoolOpts, Resources) ->
  courier_pool:start(PoolRef, PoolOpts),
  io:format("~p~n", [Resources]),
  ok.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
