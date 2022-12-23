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
    Req :: binary().
dispatch_req(PoolRef, RawReq) ->
  ReqUri = courier_req:get(uri, RawReq),
  lager:info("URI TEST ~p", [ReqUri]),
  case courier_resource:match(PoolRef, ReqUri) of
    {ok, Resource} ->
      Req = courier_req:parse(RawReq),
      lager:info("Resource match for uri ~p", [Resource]),
      courier_handler:execute(Req, Resource);
    nomatch ->
      lager:info("No resource match for uri ~p", [ReqUri]),
      courier_res:error(404)
  end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
