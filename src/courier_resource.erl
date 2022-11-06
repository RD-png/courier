%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created :  6 Nov 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(courier_resource).
-author("ryandenby").

%% API
-export([]).

-type resource()  :: {URI         :: binary(),
                      Handler     :: module(),
                      HandlerArgs :: [term()]}.
-type resources() :: [resource()].
-export_type([resources/0]).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
