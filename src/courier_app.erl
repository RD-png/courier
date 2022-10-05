%%%-------------------------------------------------------------------
%% @doc courier public API
%% @end
%%%-------------------------------------------------------------------

-module(courier_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    courier_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
