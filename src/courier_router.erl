%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created :  25 Nov 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(courier_router).
-author("ryandenby").

%% API
-export([dispatch_req/2]).

-type http_error() :: string().

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

-spec dispatch_req(PoolRef, Req) -> ok | http_error() when
    PoolRef :: atom(),
    Req     :: binary().
dispatch_req(PoolRef, Req) ->
  Uri = courier_req:uri(Req),
  lager:info("URI TEST ~p", [Uri]),
  case courier_resource:match(PoolRef, Uri) of
    {ok, Resource} ->
      lager:debug("Resource match for uri ~p", [Resource]),
      courier_handler:execute(Uri, Resource);
    nomatch ->
      lager:debug("No resource match for uri ~p", [Uri]),
      courier_res:error(404)
  end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
