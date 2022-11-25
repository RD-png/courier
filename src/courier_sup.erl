%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%% Courier top level supervisor.
%%% @end
%%% Created :  5 Oct 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(courier_sup).
-author("ryandenby").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%-------------------------------------------------------------------
%% API
%%%-------------------------------------------------------------------

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%-------------------------------------------------------------------
%% Supervisor callbacks
%%%-------------------------------------------------------------------

init([]) ->
  courier_resource:init_resource_table(),
  SupFlags   = #{strategy  => one_for_one,
                 intensity => 1,
                 period    => 5},
  ChildSpecs = [courier_acceptor_sup:get_spec(),
                courier_connection_sup:get_spec()],

  {ok, {SupFlags, ChildSpecs}}.
