%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%% irc top level supervisor.
%%% @end
%%% Created :  6 Aug 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(courier_acceptor_sup).
-author("ryandenby").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1,
         get_spec/0]).

-define(SERVER, ?MODULE).

%%%-------------------------------------------------------------------
%% API
%%%-------------------------------------------------------------------

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

get_spec() ->
  #{id       => ?MODULE,
    start    => {?MODULE, start_link, []},
    restart  => permanent,
    shutdown => 5000,
    type     => supervisor,
    modules  => [?MODULE]}.

%%%-------------------------------------------------------------------
%% Supervisor callbacks
%%%-------------------------------------------------------------------

init([]) ->
  SupFlags   = #{strategy  => simple_one_for_one,
                 intensity => 1,
                 period    => 5},
  ChildSpecs = [courier_acceptor:get_spec()],

  {ok, {SupFlags, ChildSpecs}}.

%%%-------------------------------------------------------------------
%% Internal functions
%%%-------------------------------------------------------------------
