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

-type uri_var_map() :: #{UriPatternKey :: atom => UriVar :: term()}.

-type http_error() :: string().

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

-spec dispatch_req(PoolRef :: atom(), Req :: binary()) -> ok | http_error().
dispatch_req(PoolRef, Req) ->
  Resources = courier_resource:pool_fetch_all_resources(PoolRef),
  ReqUri = get_req_uri(Req),
  lager:info("URI TEST ~p", [ReqUri]),
  case resource_match_uri(Resources, ReqUri) of
    {error, missing_uri_resource} ->
      http_error(404);
    ResourceMatch ->
      try_dispatch(ResourceMatch)
  end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

-spec try_dispatch({Handler :: module(), UriVarMap :: uri_var_map(),
                    HandlerArgs :: term()}) ->
        ok | http_error().
try_dispatch({Handler, UriVarMap, HandlerArgs}) ->
  try
  Handler:execute(UriVarMap, HandlerArgs),
  ok
  catch
    error:undef ->
      lager:error("Missing handler ~p", [Handler]),
      http_error(500)
  end.

-spec resource_match_uri(Resources :: [courier_resource:resource()],
                         Uri       :: string()) ->
        {Handler :: module(), UriVarMap :: uri_var_map(), HandlerArgs :: term()}
          | {error, missing_uri_resource}.
resource_match_uri(Resources, Uri) ->
  %% REVIEW: Matcning against record here is not ideal, change to use map
  ResourceMatchFn =
    fun({_, {UriPattern, UriPatternKeys}, Handler, HandlerArgs}) ->
        case re:run(Uri, UriPattern, [global, trim_all]) of
          {match, UriVars} ->
            UriVarMap = get_uri_var_map(UriPatternKeys, UriVars),
            {value, {Handler, UriVarMap, HandlerArgs}};
          nomatch ->
            false
        end
    end,
  case lists:search(ResourceMatchFn, Resources) of
    {value, Match} ->
      Match;
    false ->
      {error, missing_uri_resource}
  end.

-spec get_uri_var_map(UriPatternKeys :: [binary()], UriVars :: [term()]) ->
        uri_var_map().
get_uri_var_map(UriPatternKeys, UriVars) ->
  maps:from_list(lists:zip(UriPatternKeys, UriVars)).

get_req_uri(Req) ->
  [UriSegment, _Rest] = binary:split(Req, <<"\r\n">>),
  [_Method, Uri, _HTTPVer] = binary:split(UriSegment, <<" ">>, [global]),
  binary_to_list(Uri).

http_error(500) -> "500 Internal server error";
http_error(404) -> "404 Not Found".
