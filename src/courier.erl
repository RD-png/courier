%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(courier).
-author("ryandenby").

%% API
-export([listen/2,
         close/1]).

-export_type([listen_opts/0]).
-type listen_opts() :: #{port          => port(),
                         num_listeners => pos_integer()}.

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

-spec listen(PortRef :: atom(), ListenOpts :: listen_opts()) ->
        supervisor:startchild_ret().
listen(PortRef, ListenOpts) ->
  courier_acceptor_sup:start_pool(PortRef, ListenOpts).

-spec close(PortRef :: atom()) -> ok | {error, not_found | simple_one_for_one}.
close(PortRef) ->
  courier_acceptor_sup:close_pool(PortRef).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
