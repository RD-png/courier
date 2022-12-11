%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%% Courier HTTP request module
%%% @end
%%% Created :  11 Dec 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(courier_req).

-export([get/2]).

%%%-------------------------------------------------------------------
%% API
%%%-------------------------------------------------------------------

get(uri, Req) ->
  [UriSegment, _Rest] = binary:split(Req, <<"\r\n">>),
  [_Method, Uri, _HTTPVer] = binary:split(UriSegment, <<" ">>, [global]),
  Uri.

%%%-------------------------------------------------------------------
%% Internal functions
%%%-------------------------------------------------------------------
