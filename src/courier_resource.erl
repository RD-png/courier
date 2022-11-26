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

-record resource_spec, {uri     :: atom(),
                        pool    :: atom(),
                        spec    :: uri_spec(),
                        handler :: module(),
                        args    :: term()}.

-type resource_spec() :: #resource_spec{}.

-type uri_spec() :: {UriPattern     :: term(),
                     UriPatternKeys :: [binary()]}.

-type resource() :: {UriRef      :: atom(),
                     UriRegex    :: iodata(),
                     Handler     :: module(),
                     HandlerArgs :: term()}.
-export_type([resource/0]).

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
  %% REVIEW: Testing match specs
  Matches = ets:select(?TABLE, [{{resource_spec, '_', '$1', '_', '_', '_'},
                                 [{'=:=', '$1', PoolRef}],
                                 ['$_']}]),
  case Matches of
    [Resources] ->
      Resources;
    [] ->
      undefined
  end.

-spec fetch_resource(PoolRef :: atom(), UriRef :: binary()) ->
        resource() | undefined.
fetch_resource(PoolRef, UriRef) ->
  %% REVIEW: Testing match specs
  Matches = ets:select(?TABLE, [{{resource_spec, '$1', '$2', '_', '_', '_'},
                                 [{'andalso',
                                   {'=:=', '$1', UriRef},
                                   {'=:=', '$2', PoolRef}}],
                                 ['$_']}]),
  case Matches of
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
      ResourceSpec = #resource_spec{uri     = UriRef,
                                    pool    = PoolRef,
                                    spec    = UriSpec,
                                    handler = Handler,
                                    args    = HandlerArgs},
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
