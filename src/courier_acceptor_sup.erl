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
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1,
         get_spec/2]).

-define(SERVER, ?MODULE).

%%%-------------------------------------------------------------------
%% API
%%%-------------------------------------------------------------------

start_link(ListenOpts) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [ListenOpts]).

get_spec(PortRef, ListenOpts) ->
  #{id       => {PortRef, ?MODULE},
    start    => {?MODULE, start_link, [ListenOpts]},
    restart  => permanent,
    shutdown => 5000,
    type     => supervisor,
    modules  => [?MODULE]}.

%%%-------------------------------------------------------------------
%% Supervisor callbacks
%%%-------------------------------------------------------------------

init([ListenOpts]) ->
  Port         = maps:get(port, ListenOpts),
  NumListeners = maps:get(num_listeners, ListenOpts),

  SupFlags   = #{strategy  => one_for_one,
                 intensity => 1 + ceil(math:log2(NumListeners)),
                 period    => 5},

  {ok, ListenSocket} = listen_port(Port),

  ChildSpecs = [courier_acceptor:get_spec(Id, ListenSocket)
                || Id <- lists:seq(1, NumListeners)],

  {ok, {SupFlags, ChildSpecs}}.

%%%-------------------------------------------------------------------
%% Internal functions
%%%-------------------------------------------------------------------

listen_port(Port) ->
  gen_tcp:listen(0, [{port, Port}]).
