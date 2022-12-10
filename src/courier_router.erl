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
dispatch_req(PoolRef, Req) ->
  ReqUri = get_req_uri(Req),
  lager:info("URI TEST ~p", [ReqUri]),
  case courier_resource:match(PoolRef, ReqUri) of
    nomatch ->
      http_error(404);
    Resource ->
      try_dispatch(Resource)
  end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

try_dispatch({UriVarMap, Handler, HandlerArgs}) ->
  try
  Handler:execute(UriVarMap, HandlerArgs)
  catch
    error:undef ->
      lager:error("Missing handler ~p", [Handler]),
      http_error(500)
  end.

get_req_uri(Req) ->
  [UriSegment, _Rest] = binary:split(Req, <<"\r\n">>),
  [_Method, Uri, _HTTPVer] = binary:split(UriSegment, <<" ">>, [global]),
  Uri.

http_error(500) -> "500 Internal server error";
http_error(404) -> "404 Not Found".
