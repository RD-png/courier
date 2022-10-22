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

-spec start_link(Socket :: inet:socket()) -> {ok, Pid :: pid()}.
start_link(Socket) ->
  Pid = spawn_link(?MODULE, init, [Socket]),
  {ok, Pid}.

-spec init(Socket :: inet:socket()) -> no_return().
init(Socket) ->
  connect(Socket).

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

connect(Socket) ->
  receive
    connected ->
      inet:setopts(Socket, [{active, once}]),
      handle(Socket)
  after ?TIMEOUT ->
      lager:error("Connection timeout on socket ~p", [Socket]),
      exit({error, timeout})
  end.

handle(Socket) ->
  receive
    {tcp, Socket, Msg} ->
      inet:setopts(Socket, [{active, once}]),
      gen_tcp:send(Socket, Msg),
      lager:info("recieved ~p", [Msg]),
      ok;
    {tcp_closed, Socket} ->
      lager:info("Connection on socket ~p closed, closing connection",
                 [Socket]),
      ok;
    {tcp_error, Socket, Reason} ->
      lager:error("Connection closing on socket ~p with error: ~p",
                  [Socket, Reason]),
      exit({tcp_error, Reason})
  end.
