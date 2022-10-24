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
-export([start_all/0,
         start/1,
         start/2,
         stop/1,
         restart/1]).

-define(ACCEPTOR_SUP, courier_acceptor_sup).

-define(CHILD_ID(PoolRef), {courier_acceptor_pool_sup, PoolRef}).

-export_type([listen_opts/0]).
-type listen_opts() :: #{port          => port(),
                         num_listeners => pos_integer()}.

-type start_pool_ret() :: supervisor:startchild_ret()
                        | {error, invalid_pool_spec}
                        | {error, pools_missing}
                        | {error, invalid_opts}.

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% TODO
-spec start_all() -> start_pool_ret().
start_all() ->
  do_start([]).

%% @doc Start acceptor pool defined in application env.
-spec start(PoolRef :: atom()) -> start_pool_ret().
start(PoolRef) ->
  do_start([PoolRef]).

%% @doc Start a new acceptor pool.
-spec start(PoolRef :: atom(), ListenOpts :: listen_opts()) ->
        supervisor:startchild_ret() | {error, invalid_opts}.
start(PoolRef, ListenOpts) when is_map(ListenOpts) ->
  Port = maps:get(port, ListenOpts),
  PoolSpec = courier_acceptor_pool_sup:get_spec(PoolRef, ListenOpts),
  case supervisor:start_child(?ACCEPTOR_SUP, PoolSpec) of
    {ok, Child} = Res ->
      lager:info("Acceptor pool started at '~p', listening on port ~p",
                 [Child, Port]),
      Res;
     {error, Reason} = Err ->
      lager:warning("Acceptor pool '~p', failed to start listening on port
 ~p, with error: ~p", [Port, Reason]),
      Err
  end;
start(_PoolRef, _InvalidOpts) ->
  {error, invalid_opts}.


%% @doc Stop an existing acceptor pool.
-spec stop(PoolRef :: atom()) ->
        {ok, PoolRef :: atom()} | {error, Reason :: not_found
                                                  | simple_one_for_one}.
stop(PoolRef) ->
  case supervisor:terminate_child(?ACCEPTOR_SUP, ?CHILD_ID(PoolRef)) of
    ok ->
      lager:info("Acceptor pool '~p' closed", [PoolRef]),
      {ok, PoolRef};
    {error, Reason} = Err ->
      lager:warning("Acceptor pool '~p', failed to close with error: ~p",
                    [PoolRef, Reason]),
      Err
  end.

%% @doc Restart an existing acceptor pool if the id `PoolRef' is defined
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
      lager:info("Acceptor pool ~p has been restarted", [PoolRef]),
      {ok, PoolRef};
    {error, Reason} = Err ->
      lager:info("Acceptor pool ~p failed to restart with error: ~p",
                 [PoolRef, Reason]),
      Err
  end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

do_start(Pools) ->
  case application:get_env(courier, pools) of
    {ok, EnvPools} ->
      do_start(Pools, EnvPools);
    undefined ->
      {error, pools_missing}
  end.

do_start([], _EnvPools) ->
  ok;
do_start([Pool | Pools], EnvPools) ->
  case proplists:get_value(Pool, EnvPools) of
    undefined ->
      {error, invalid_pool_spec};
    ListenOpts ->
      start(Pool, ListenOpts),
      do_start(Pools, EnvPools)
  end.
