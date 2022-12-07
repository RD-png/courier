%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created :  17 Oct 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(courier_connection).
-author("ryandenby").

%% API
-export([start_link/2,
         get_spec/0]).

-export([init/2]).

-define(TIMEOUT, courier_app:get_env_or_default(connection_timeout, 10000)).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

-spec start_link(Socket :: inet:socket(), PoolRef :: atom()) ->
        {ok, Pid :: pid()}.
start_link(Socket, PoolRef) ->
  Pid = spawn_link(?MODULE, init, [Socket, PoolRef]),
  {ok, Pid}.

-spec init(Socket :: inet:socket(), PoolRef :: atom()) -> no_return().
init(Socket, PoolRef) ->
  connect(Socket, PoolRef).

-spec get_spec() -> supervisor:child_spec().
get_spec() ->
  #{id       => ?MODULE,
    start    => {?MODULE, start_link, []},
    restart  => transient,
    shutdown => 5000,
    type     => worker,
    modules  => [?MODULE]}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

connect(Socket, PoolRef) ->
  receive
    connected ->
      inet:setopts(Socket, [{active, once}]),
      handle(Socket, PoolRef)
  after ?TIMEOUT ->
      lager:error("Connection timeout on socket ~p", [Socket]),
      exit({error, timeout})
  end.

handle(Socket, PoolRef) ->
  receive
    {tcp, Socket, Req} ->
      lager:info("Pool ~p received request ~p", [PoolRef, Req]),
      inet:setopts(Socket, [{active, once}]),
      Response = courier_router:dispatch_req(PoolRef, Req),
      gen_tcp:send(Socket, Response),
      handle(Socket, PoolRef);
    {tcp_closed, Socket} ->
      lager:info("Connection on socket ~p closed, closing connection",
                 [Socket]),
      ok;
    {tcp_error, Socket, Reason} ->
      lager:error("Connection closing on socket ~p with error: ~p",
                  [Socket, Reason]),
      exit({tcp_error, Reason})
  end.
