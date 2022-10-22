%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created :  13 Oct 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(courier_pool).
-author("ryandenby").

%% API
-export([start/2,
         stop/1,
         restart/1]).

-define(ACCEPTOR_SUP, courier_acceptor_sup).

-define(CHILD_ID(PoolRef), {courier_acceptor_pool_sup, PoolRef}).

-export_type([listen_opts/0]).
-type listen_opts() :: #{port          => port(),
                         num_listeners => pos_integer()}.

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Start a new listener pool
-spec start(PoolRef :: atom(), ListenOpts :: listen_opts()) ->
        supervisor:startchild_ret().
start(PoolRef, ListenOpts) ->
  Port = maps:get(port, ListenOpts),
  PoolSpec = courier_acceptor_pool_sup:get_spec(PoolRef, ListenOpts),
  case supervisor:start_child(?ACCEPTOR_SUP, PoolSpec) of
    {ok, Child} = Res ->
      lager:info("Listener pool started at '~p', listening on port ~p",
                 [Child, Port]),
      Res;
     {error, Reason} = Err ->
      lager:warning("Listener pool '~p', failed to start listening on port
 ~p, with error: ~p", [Port, Reason]),
      Err
  end.

%% @doc Stop an existing listener pool
-spec stop(PoolRef :: atom()) ->
        {ok, PoolRef :: atom()} | {error, Reason :: not_found
                                                  | simple_one_for_one}.
stop(PoolRef) ->
  case supervisor:terminate_child(?ACCEPTOR_SUP, ?CHILD_ID(PoolRef)) of
    ok ->
      lager:info("Listener pool '~p' closed", [PoolRef]),
      {ok, PoolRef};
    {error, Reason} = Err ->
      lager:warning("Listener pool '~p', failed to close with error: ~p",
                    [PoolRef, Reason]),
      Err
  end.

%% @doc Restart an existing listener pool if the id `PoolRef' is defined
%% in the `courier_acceptor_sup' child spec.
-spec restart(PoolRef :: atom()) ->
        {ok, PoolRef :: atom()} | {error, Reason :: running
                                                  | restarting
                                                  | not_found
                                                  | simple_one_for_one
                                                  | term()}.
restart(PoolRef) ->
  case supervisor:restart_child(?ACCEPTOR_SUP, ?CHILD_ID(PoolRef)) of
    {ok, _Child} ->
      lager:info("Listener pool ~p has been restarted", [PoolRef]),
      {ok, PoolRef};
    {error, Reason} = Err ->
      lager:info("Listener pool ~p failed to restart with error: ~p",
                 [PoolRef, Reason]),
      Err
  end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
