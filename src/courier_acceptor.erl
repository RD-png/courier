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
-export([start_link/2,
         get_spec/3]).

-export([init/2]).

-type id() :: pos_integer().

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

-spec start_link(ListenSocket :: inet:socket(),
                 Resources    :: courier_resource:resources()) ->
        {ok, Pid :: pid()}.
start_link(ListenSocket, Resources) ->
  Pid = spawn_link(?MODULE, init, [ListenSocket, Resources]),
  {ok, Pid}.

-spec init(ListenSocket :: inet:socket(),
           Resources    :: courier_resource:resources()) -> no_return().
init(ListenSocket, Resources) ->
  accept(ListenSocket, Resources).

%% @doc Create a child spec for the module, multiple instances of this module
%% will be spawned, so `Id' is used to create a unique child id. The spawned
%% child will listen for connections on the socket `ListenSocket'.
-spec get_spec(Id :: id(), ListenSocket :: inet:socket(),
               Resources :: courier:resources()) -> supervisor:child_spec().
get_spec(Id, ListenSocket, Resources) ->
  #{id       => {?MODULE, Id},
    start    => {?MODULE, start_link, [ListenSocket, Resources]},
    restart  => permanent,
    shutdown => brutal_kill,
     type     => worker,
    modules  => [?MODULE]}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

accept(ListenSocket, Resources) ->
  case gen_tcp:accept(ListenSocket, infinity) of
    {ok, Socket} ->
      lager:info("courier: accepted connection on: ~p", [Socket]),
      {ok, Pid} = courier_connection_sup:create_connection(Socket, Resources),
      ok = gen_tcp:controlling_process(Socket, Pid),
      Pid ! connected;
    {error, closed} = Err ->
      lager:error("Socket ~p has closed, closing acceptor process",
                  [ListenSocket]),
      exit(Err);
    {error, Reason} ->
      lager:error("courier: acceptor failed with reason: ~p", [Reason])
  end,
  accept(ListenSocket, Resources).
