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
-export([start_all/0, start/1, start/2, stop/1, restart/1, is_pool_active/1]).

-define(ACCEPTOR_SUP, courier_acceptor_sup).

-define(POOL_SUP_NAME(Ref), list_to_atom(atom_to_list(Ref) ++ "_pool")).

-define(CHILD_ID(Ref), {courier_acceptor_pool_sup, ?POOL_SUP_NAME(Ref)}).

-type pool_opts() :: #{port      => port(),
                       acceptors => pos_integer(),
                       resources => [courier_resource:resource()]}.
-export_type([pool_opts/0]).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Start all acceptor pools defined in the application environment,
%% pools that have already been started will be skipped.
%% @throws {invalid_opts, {PoolRef, InvalidOpts}}
-spec start_all() -> ok | {error, Reason} when
    Reason :: pools_missing.
start_all() ->
  start_env_defined_pool(all).

%% @doc Start acceptor pool defined in application environment.
%% @throws {invalid_opts, {PoolRef, InvalidOpts}}
-spec start([PoolRef] | PoolRef) -> ok | {error, Reason} when
    PoolRef :: atom(),
    Reason :: pools_missing | {undefined_pool_spec, PoolRef}.
start(PoolRefs) when is_list(PoolRefs) ->
  start_env_defined_pool(PoolRefs);
start(PoolRef) when is_atom(PoolRef) ->
  start_env_defined_pool([PoolRef]).

%% @doc Start a new acceptor pool.
%% @throws {invalid_opts, {PoolRef, InvalidOpts}}
-spec start(PoolRef, PoolOpts) -> supervisor:startchild_ret() when
    PoolRef :: atom(),
    PoolOpts :: pool_opts().
start(PoolRef, #{port := Port} = PoolOpts) ->
  PoolSpec = courier_acceptor_pool_sup:get_spec(?POOL_SUP_NAME(PoolRef),
                                                PoolOpts),
  case supervisor:start_child(?ACCEPTOR_SUP, PoolSpec) of
    {ok, Child} = Res ->
      lager:info("Acceptor pool started at '~p', listening on port ~p",
                 [Child, Port]),
      Res;
     {error, Reason} = Err ->
      lager:warning("Acceptor pool '~p', failed to start listening on port
~p, with error: ~p", [PoolRef, Port, Reason]),
      Err
  end;
start(PoolRef, InvalidOpts) ->
  throw({invalid_opts, {PoolRef, InvalidOpts}}).

%% @doc Stop an existing acceptor pool.
-spec stop(PoolRef) -> {ok, PoolRef} | {error, Reason} when
    PoolRef :: atom(),
    Reason :: not_found.
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

%% @doc Restart an existing acceptor pool if the id `PoolRef' is defined in
%% the `courier_acceptor_sup' child spec.
-spec restart(PoolRef) -> {ok, PoolRef} | {error, Reason} when
    PoolRef :: atom(),
    Reason :: running | restarting | not_found | term().
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

%% @doc Check if a pool is active and accepting connections.
-spec is_pool_active(PoolRef) -> boolean() when
    PoolRef :: atom().
is_pool_active(PoolRef) ->
  case whereis(?POOL_SUP_NAME(PoolRef)) of
    undefined ->
      false;
    _Pid ->
      true
  end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

start_env_defined_pool(Pools) ->
  case application:get_env(courier, pools) of
    {ok, EnvPools} ->
      case Pools of
        all ->
          start_all_env_defined_pool(EnvPools);
        Pools ->
          start_env_defined_pool(Pools, EnvPools)
      end;
    undefined ->
      {error, pools_missing}
  end.

start_env_defined_pool([], _EnvPools) ->
  ok;
start_env_defined_pool([Pool | Pools], EnvPools) ->
  case proplists:get_value(Pool, EnvPools) of
    undefined ->
      {error, {undefined_pool_spec, Pool}};
    PoolOpts ->
      start(Pool, PoolOpts),
      start_env_defined_pool(Pools, EnvPools)
  end.

start_all_env_defined_pool(EnvPools) ->
  [start(Pool, PoolOpts) || {Pool, PoolOpts} <- EnvPools],
  ok.
