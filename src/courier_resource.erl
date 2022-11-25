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
-export([init_resource_table/0 , delete_resource_table/0]).

-export([fetch_resource/2, fetch_all_resources/1, add_resource/2,
         add_resources/2]).

-type uri_spec() :: {UriPattern     :: term(),
                     UriPatternKeys :: [binary()]}.

-type resource() :: {UriRef      :: atom(),
                     UriRegex    :: iodata(),
                     Handler     :: module(),
                     HandlerArgs :: term()}.
-export_type([resource/0]).

-type resource_spec() :: {{PoolRef :: atom(), UriRef :: atom()},
                          UriSpec     :: uri_spec(),
                          Handler     :: module(),
                          HandlerArgs :: term()}.

-define(TABLE, pool_resources).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Create an ets table to store the resoruces for acceptor pools.
-spec init_resource_table() -> ok | table_already_registered.
init_resource_table() ->
  case is_pool_resource_table_registered() of
    true ->
      table_already_exists;
    false ->
      ets:new(?TABLE, [set, public, named_table, {read_concurrency, true},
                       {write_concurrency, true}]),
      ok
  end.

%% @doc Delete a resource table for a registered pool.
-spec delete_resource_table() -> ok | table_not_registered.
delete_resource_table() ->
  case is_pool_resource_table_registered() of
    true ->
      ets:delete(?TABLE);
    false ->
      table_not_registered
  end.

%% REVIEW: Possibly move to the pool module
-spec fetch_all_resources(PoolRef :: atom()) -> [resource()] | undefined.
fetch_all_resources(PoolRef) ->
  case ets:match(?TABLE, {{PoolRef, '_'}, '_', '_', '_'}) of
    [Resources] ->
      Resources;
    [] ->
      undefined
  end.

-spec fetch_resource(PoolRef :: atom(), UriRef :: binary()) ->
        resource() | undefined.
fetch_resource(PoolRef, UriRef) ->
  case ets:lookup(?TABLE, {PoolRef, UriRef}) of
    [Resource] ->
      Resource;
    [] ->
      undefined
    end.

-spec add_resource(PoolRef :: atom(), Resource :: resource()) ->
        ok | {error, invalid_uri_regex | resource_exists}.
add_resource(PoolRef, {UriRef, UriRegex, Handler, HandlerArgs} = _Resource) ->
  case create_uri_spec(UriRegex) of
    {error, invalid_uri_regex} = Err ->
      Err;
    {_UriPattern, _UriPatternKeys} = UriSpec ->
      ResourceSpec = {{PoolRef, UriRef}, UriSpec, Handler, HandlerArgs},
      try_add_resource(ResourceSpec)
  end.

-spec add_resources(PoolRef :: atom(), Resources :: [resource()]) -> ok.
add_resources(PoolRef, Resources) ->
  lists:foreach(fun(Resource) ->
                    ok = add_resource(PoolRef, Resource)
                end, Resources).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

-spec create_uri_spec(UriRegex :: iodata()) ->
        uri_spec() | {error, invalid_uri_regex}.
create_uri_spec(UriRegex) ->
  case re:compile(UriRegex) of
    {ok, UriPattern} ->
      {namelist, UriPatternKeys} = re:inspect(UriPattern, namelist),
      {UriPattern, UriPatternKeys};
    {error, _Reason} ->
      {error, invalid_uri_regex}
  end.

-spec is_pool_resource_table_registered() -> boolean().
is_pool_resource_table_registered() ->
  case ets:whereis(?TABLE) of
    undefined ->
      false;
    _Tid ->
      true
  end.

-spec try_add_resource(ResourceSpec :: resource_spec()) ->
        ok | {error, resource_exists}.
try_add_resource(ResourceSpec) ->
  case ets:insert_new(?TABLE, ResourceSpec) of
    true ->
      ok;
    false ->
      {error, resource_exists}
  end.
