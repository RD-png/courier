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

-spec start_link(Socket    :: inet:socket(),
                 Resources :: [courier_resource:resource()]) ->
        {ok, Pid :: pid()}.
start_link(Socket, Resources) ->
  Pid = spawn_link(?MODULE, init, [Socket, Resources]),
  {ok, Pid}.

-spec init(Socket    :: inet:socket(),
           Resources :: [courier_resource:resource()]) -> no_return().
init(Socket, Resources) ->
  connect(Socket, Resources).

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

connect(Socket, Resources) ->
  receive
    connected ->
      inet:setopts(Socket, [{active, once}]),
      handle(Socket, Resources)
  after ?TIMEOUT ->
      lager:error("Connection timeout on socket ~p", [Socket]),
      exit({error, timeout})
  end.

handle(Socket, Resources) ->
  receive
    {tcp, Socket, Msg} ->
      inet:setopts(Socket, [{active, once}]),
      gen_tcp:send(Socket, Msg),
      lager:info("recieved ~p, handlers ~p", [Msg, Resources]),
      handle(Socket, Resources);
    {tcp_closed, Socket} ->
      lager:info("Connection on socket ~p closed, closing connection",
                 [Socket]),
      ok;
    {tcp_error, Socket, Reason} ->
      lager:error("Connection closing on socket ~p with error: ~p",
                  [Socket, Reason]),
      exit({tcp_error, Reason})
  end.
