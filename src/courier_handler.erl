%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%% Courier handler behaviour
%%% @end
%%% Created :  10 Dec 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(courier_handler).

-export([execute/2]).

-type http_response() :: {ok, binary()}.

%%%-------------------------------------------------------------------
%% Callbacks
%%%-------------------------------------------------------------------

-callback handle(Req, UriVarMap, HandlerArgs) -> http_response() when
    Req :: binary(),
    UriVarMap :: courier_resource:uri_var_map(),
    HandlerArgs :: term().

%%%-------------------------------------------------------------------
%% API
%%%-------------------------------------------------------------------

execute(Req, {UriVarMap, Handler, HandlerArgs}) ->
  try Handler:handle(Req, UriVarMap, HandlerArgs) of
    {ok, Response} ->
      courier_res:success(Response)
    catch Class:Reason:Stacktrace ->
        erlang:raise(Class, Reason, Stacktrace)
    end.
