%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%% Courier HTTP response module
%%% @end
%%% Created :  11 Dec 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(courier_res).

-export([success/1, error/1]).

%%%-------------------------------------------------------------------
%% API
%%%-------------------------------------------------------------------

success(Res) ->
  Res.

error(404) -> <<"HTTP/1.1 404 Not Found\r\nContent-Length: 19\r\n
Connection: close\r\n">>;
error(500) -> <<"HTTP/1.1 500 Internal Server Error\r\nContent-Length: 30\r\n
Connection: close\r\n">>.

%%%-------------------------------------------------------------------
%% Internal functions
%%%-------------------------------------------------------------------
