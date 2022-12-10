%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%% Courier handler behaviour
%%% @end
%%% Created :  10 Dec 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(courier_handler).

-type http_response() ::
        {StatusCode :: pos_integer(),
         ResponseHeaders :: #{Headers :: binary() => Value :: binary()},
         ResponseBody :: term()}.

%%%-------------------------------------------------------------------
%% Callbacks
%%%-------------------------------------------------------------------

-callback pre_execute(Req, UriVarMap, HandlerArgs) -> Response when
    Req :: binary(),
    UriVarMap :: courier_resource:uri_var_map(),
    HandlerArgs :: term(),
    Response :: {Action, {Req, UriVarMap, HandlerArgs}},
    Action :: ok | stop.

-callback execute(Req, UriVarMap, HandlerArgs) -> http_response() when
    Req :: binary(),
    UriVarMap :: courier_resource:uri_var_map(),
    HandlerArgs :: term().

-callback post_execute(Req, UriVarMap, HandlerArgs) -> Response when
    Req :: binary(),
    UriVarMap :: courier_resource:uri_var_map(),
    HandlerArgs :: term(),
    Response :: {ok, {Req, UriVarMap, HandlerArgs}}.

-optional_callbacks([pre_execute/3, post_execute/3]).

%%%-------------------------------------------------------------------
%% API
%%%-------------------------------------------------------------------
