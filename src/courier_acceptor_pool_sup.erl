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

-define(pool_sup_name(Ref), list_to_atom(atom_to_list(Ref) ++ "_pool")).

%%%-------------------------------------------------------------------
%% API
%%%-------------------------------------------------------------------

start_link(PoolRef, ListenOpts) ->
  supervisor:start_link({local, ?pool_sup_name(PoolRef)}, ?MODULE, [PoolRef, ListenOpts]).

-spec get_spec(PoolRef :: atom(), ListenOpts :: courier:listen_opts()) ->
        supervisor:child_spec().
get_spec(PoolRef, ListenOpts) ->
  #{id       => {?MODULE, PoolRef},
    start    => {?MODULE, start_link, [PoolRef, ListenOpts]},
    restart  => permanent,
    shutdown => 5000,
    type     => supervisor,
    modules  => [?MODULE]}.

%%%-------------------------------------------------------------------
%% Supervisor callbacks
%%%-------------------------------------------------------------------

init([_PoolRef, ListenOpts]) ->
  Port         = maps:get(port, ListenOpts),
  NumListeners = maps:get(num_listeners, ListenOpts),
  {ok, ListenSocket} = listen_port(Port),

  SupFlags   = #{strategy  => one_for_one,
                 intensity => 1,
                 period    => 5},
  ChildSpecs = [courier_acceptor:get_spec(Id, ListenSocket)
                || Id <- lists:seq(1, NumListeners)],

  {ok, {SupFlags, ChildSpecs}}.

%%%-------------------------------------------------------------------
%% Internal functions
%%%-------------------------------------------------------------------

listen_port(Port) ->
  gen_tcp:listen(0, [{port, Port}]).
