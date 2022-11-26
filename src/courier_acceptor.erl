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

-type id() :: pos_integer().

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

-spec start_link(ListenSocket :: inet:socket()) -> {ok, Pid :: pid()}.
start_link(ListenSocket) ->
  Pid = spawn_link(?MODULE, init, [ListenSocket]),
  {ok, Pid}.

-spec init(ListenSocket :: inet:socket()) -> no_return().
init(ListenSocket) ->
  accept(ListenSocket).

%% @doc Create a child spec for the module, multiple instances of this
%% module will be spawned, so `Id' is used to create a unique child id. The
%% spawned child will listen for connections on the socket `ListenSocket'.
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
  case gen_tcp:accept(ListenSocket, infinity) of
    {ok, Socket} ->
      lager:info("courier: accepted connection on: ~p", [Socket]),
      {ok, ConnPid} = courier_connection_sup:create_connection(Socket),
      ok = gen_tcp:controlling_process(Socket, ConnPid),
      ConnPid ! connected;
    {error, closed} = Err ->
      lager:error("Socket ~p has closed, closing acceptor process",
                  [ListenSocket]),
      exit(Err);
    {error, Reason} ->
      lager:error("courier: acceptor failed with reason: ~p", [Reason])
  end,
  accept(ListenSocket).
