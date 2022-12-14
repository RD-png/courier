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
-export([start_link/2, get_spec/3]).

-export([init/2]).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

-spec start_link(ListenSocket, PoolRef) -> {ok, Pid} when
    ListenSocket :: inet:socket(),
    PoolRef :: atom(),
    Pid :: pid().
start_link(ListenSocket, PoolRef) ->
  Pid = spawn_link(?MODULE, init, [ListenSocket, PoolRef]),
  {ok, Pid}.

-spec init(ListenSocket, PoolRef) -> no_return() when
    ListenSocket :: inet:socket(),
    PoolRef :: atom().
init(ListenSocket, PoolRef) ->
  accept(ListenSocket, PoolRef).

%% @doc Create a child spec for the module, multiple instances of this
%% module will be spawned, so `Id' is used to create a unique child id. The
%% spawned child will listen for connections on the socket `ListenSocket'.
-spec get_spec(Id , ListenSocket, PoolRef) -> supervisor:child_spec() when
    Id :: pos_integer(),
    ListenSocket :: inet:socket(),
    PoolRef :: atom().
get_spec(Id, ListenSocket, PoolRef) ->
  #{id       => {?MODULE, Id},
    start    => {?MODULE, start_link, [ListenSocket, PoolRef]},
    restart  => permanent,
    shutdown => brutal_kill,
     type     => worker,
    modules  => [?MODULE]}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

accept(ListenSocket, PoolRef) ->
  case gen_tcp:accept(ListenSocket, infinity) of
    {ok, Socket} ->
      lager:info("courier: accepted connection on: ~p", [Socket]),
      {ok, Pid} = courier_connection_sup:create_connection(Socket, PoolRef),
      ok = gen_tcp:controlling_process(Socket, Pid),
      Pid ! connected;
    {error, closed} = Err ->
      lager:error("Socket ~p has closed, closing acceptor process",
                  [ListenSocket]),
      exit(Err);
    {error, Reason} ->
      lager:error("courier: acceptor failed with reason: ~p", [Reason])
  end,
  accept(ListenSocket, PoolRef).
