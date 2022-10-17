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
-export([start_link/1,
         get_spec/0]).

-export([init/1]).

-define(TIMEOUT, courier_app:get_env_or_default(connection_timeout, 500)).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link(Socket) ->
  Pid = spawn_link(?MODULE, init, [Socket]),
  {ok, Pid}.

init(Socket) ->
  connect(Socket).

-spec get_spec() -> supervisor:child_spec().
get_spec() ->
  #{id       => ?MODULE,
    start    => {?MODULE, start_link, []},
    restart  => permanent,
    shutdown => 5000,
    type     => worker,
    modules  => [?MODULE]}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

connect(Socket) ->
  receive
    connected ->
      inet:setopts(Socket, [{active, once}]),
      handle(Socket)
  after ?TIMEOUT ->
      lager:error("Connection timeout on socket ~p", [Socket]),
      ok
  end.

handle(Socket) ->
  receive
    {tcp, Socket, <<"quit", _/binary>>} ->
      gen_tcp:close(Socket);
    {tcp, Socket, Msg} ->
      gen_tcp:send(Socket, Msg),
      handle(Socket)
  end.
