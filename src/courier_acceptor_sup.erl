%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc  Supervisor for `courier_acceptor_pool_sup' supervisors.
%%% Created :  10 Oct 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(courier_acceptor_sup).
-author("ryandenby").

-behaviour(supervisor).

%% API
-export([start_link/0,
         get_spec/0,
         start_pool/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%-------------------------------------------------------------------
%% API
%%%-------------------------------------------------------------------

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec get_spec() -> supervisor:child_spec().
get_spec() ->
  #{id       => ?MODULE,
    start    => {?MODULE, start_link, []},
    restart  => permanent,
    shutdown => 5000,
    type     => supervisor,
    modules  => [?MODULE]}.

-spec start_pool(PortRef :: atom(), ListenOpts :: courier:listen_opts()) -> supervisor:startchild_ret().
start_pool(PortRef, ListenOpts) ->
  Port = maps:get(port, ListenOpts),
  case supervisor:start_child(?MODULE, [PortRef, ListenOpts]) of
    {ok, Child} = Res ->
      lager:info("Listener pool started at ~p, listening on port ~p", [Child, Port]),
      Res;
     {error, Reason} = Err ->
      lager:error("Listener pool failed to start listening on port ~p, with error: ~p", [Port, Reason]),
      Err
  end.

%%%-------------------------------------------------------------------
%% Supervisor callbacks
%%%-------------------------------------------------------------------

init([]) ->
  SupFlags   = #{strategy  => simple_one_for_one,
                 intensity => 0,
                 period    => 1},
  ChildSpecs = [courier_acceptor_pool_sup:get_spec()],

  {ok, {SupFlags, ChildSpecs}}.
