%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%% Acceptor pool supervisor, spawned via `courier_acceptor_sup'.
%%% @end
%%% Created :  10 Oct 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(courier_acceptor_pool_sup).
-author("ryandenby").

-behaviour(supervisor).

%% API
-export([start_link/2,
         get_spec/2]).

%% Supervisor callbacks
-export([init/1]).

-define(POOL_SUP_NAME(Ref), list_to_atom(atom_to_list(Ref) ++ "_pool")).

%%%-------------------------------------------------------------------
%% API
%%%-------------------------------------------------------------------

start_link(PoolRef, PoolOpts) ->
  supervisor:start_link({local, ?POOL_SUP_NAME(PoolRef)},
                        ?MODULE,
                        [PoolRef, PoolOpts]).

%% @doc Create a child spec for the module, multiple instances of this
%% module will be spawned, so `PoolRef' is used to create a unique child id
%% and used to uniquely register the child. `PoolOpts' contains config
%% options for tcp and config options for `courier_acceptor' children of
%% this module.
-spec get_spec(PoolRef :: atom(), PoolOpts :: courier_pool:pool_opts()) ->
        supervisor:child_spec().
get_spec(PoolRef, PoolOpts) ->
  #{id       => {?MODULE, PoolRef},
    start    => {?MODULE, start_link, [PoolRef, PoolOpts]},
    restart  => permanent,
    shutdown => 5000,
    type     => supervisor,
    modules  => [?MODULE]}.

%%%-------------------------------------------------------------------
%% Supervisor callbacks
%%%-------------------------------------------------------------------

init([_PoolRef, #{port := Port, acceptors := NumAcceptors} = _PoolOpts]) ->
  {ok, ListenSocket} = listen_port(Port),

  SupFlags   = #{strategy  => one_for_one,
                 intensity => 1,
                 period    => 5},
  ChildSpecs = [courier_acceptor:get_spec(Id, ListenSocket)
                || Id <- lists:seq(1, NumAcceptors)],

  {ok, {SupFlags, ChildSpecs}}.

%%%-------------------------------------------------------------------
%% Internal functions
%%%-------------------------------------------------------------------

listen_port(Port) ->
  gen_tcp:listen(0, [{port, Port}]).
