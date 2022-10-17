%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created :  5 Oct 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(courier_acceptor).
-author("ryandenby").

%% API
-export([start_link/1,
         get_spec/2]).

-export([init/1]).

-type id() :: integer().

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link(ListenSocket) ->
  Pid = spawn_link(?MODULE, init, [ListenSocket]),
  {ok, Pid}.

init(ListenSocket) ->
  accept(ListenSocket),
  ok.

-spec get_spec(Id :: id(), ListenSocket :: inet:socket()) ->
        supervisor:child_spec().
get_spec(Id, ListenSocket) ->
  #{id       => {?MODULE, Id},
    start    => {?MODULE, start_link, [ListenSocket]},
    restart  => permanent,
    shutdown => brutal_kill,
    type     => worker,
    modules  => [?MODULE]}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

accept(ListenSocket) ->
  case gen_tcp:accept(ListenSocket) of
    {ok, Socket} ->
      lager:info("courier: accepted connection on: ~p", [Socket]),
      {ok, ConnectionPid} = courier_connection_sup:create_connection(Socket),
      ok = gen_tcp:controlling_process(Socket, ConnectionPid),
      ConnectionPid ! connected,
      accept(ListenSocket);
    {error, Reason} ->
      lager:debug("courier: acceptor failed with reason: ~p", [Reason]),
      accept(ListenSocket)
  end.
