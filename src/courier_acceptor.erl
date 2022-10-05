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
-export([start_link/0,
         get_spec/0]).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link() ->
  init().

get_spec() ->
  #{id       => ?MODULE,
    start    => {?MODULE, start_link, []},
    restart  => permanent,
    shutdown => 5000,
    type     => worker,
    modules  => [?MODULE]}.

init() ->
  receive
    {ok} ->
      ok
  end.
