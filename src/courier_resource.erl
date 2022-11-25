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
-export([register_resource_table/1, delete_resource_table/1]).

-export([add_resource/2, dispatch/1]).

-type uri_spec() :: {UriPattern :: iodata(),
                     UriPatternKeys ::{namelist, [binary()]}}.

-type resource() :: {UriRef      :: binary(),
                     UriRegex    :: iodata(),
                     Handler     :: module(),
                     HandlerArgs :: [term()]}.
-export_type([resource/0]).

-type resource_spec() :: {UriRef      :: binary(),
                          UriSpec     :: uri_spec(),
                          Handler     :: module(),
                          HandlerArgs :: [term()]}.

-type uri_vars() :: #{UriPatternKey :: atom => UriVar :: term()}.

-define(POOL_RESOURCE_TABLE(Ref),
        list_to_atom(atom_to_list(Ref) ++ "_resources")).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Register a new resource table for a pool if one is not
%% already registered.
-spec register_resource_table(PoolRef :: atom()) ->
        ok | {error, pool_resource_table_already_registered}.
register_resource_table(PoolRef) when is_atom(PoolRef) ->
  case
    is_pool_resource_table_registered(PoolRef) andalso
    courier_pool:is_pool_active(PoolRef)
  of
    true ->
      {error, pool_resource_table_already_registered};
    false ->
      ets:new(?POOL_RESOURCE_TABLE(PoolRef), [set, named_table,
                                              {read_concurrency, true},
                                              {write_concurrency, true}]),
      ok
  end.

%% @doc Delete a resource table for a registered pool.
-spec delete_resource_table(PoolRef :: atom()) ->
        ok | {error, pool_resource_table_not_registered}.
delete_resource_table(PoolRef) ->
  case is_pool_resource_table_registered(PoolRef) of
    true ->
      ets:delete(?POOL_RESOURCE_TABLE(PoolRef));
    false ->
      {error, pool_resource_table_not_registered}
  end.

-spec add_resource(PoolRef :: atom(), Resource :: resource()) ->
        resource_spec() | {error, Reason :: term()}.
add_resource(PoolRef, {UriRef, UriRegex, Handler, HandlerArgs} = _Resource) ->
  case create_uri_spec(UriRegex) of
    {error, _Reason} = Err->
      Err;
    {_UriPattern, _UriPatternKeys} = UriSpec ->
      %% Insert into ets or w/e
      ResourceSpec = {UriRef, UriSpec, Handler, HandlerArgs},
      ets:insert(?POOL_RESOURCE_TABLE(PoolRef), ResourceSpec),
      ResourceSpec
  end.

-spec dispatch(UriSpec :: uri_spec()) -> uri_vars().
dispatch({_UriPattern, _UriPatternKeys} = UriSpec) ->
  get_uri_variable_map(UriSpec).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

-spec create_uri_spec(UriRegex :: iodata()) ->
        uri_spec() | {error, Reason :: string()}.
create_uri_spec(UriRegex) ->
  case re:compile(UriRegex) of
    {ok, UriPattern} ->
      {namelist, UriPatternKeys} = re:inspect(UriPattern, namelist),
      {UriPattern, UriPatternKeys};
    {error, _Reason} = Err ->
      Err
  end.

-spec get_uri_variable_map(UriSpec :: uri_spec()) -> uri_vars() | {error, nomatch}.
get_uri_variable_map({UriPattern, UriPatternKeys}) ->
  case re:run(UriPattern, [global, trim_all]) of
    {match, UriVars} ->
      maps:from_list(lists:zip(UriPatternKeys, UriVars));
    nomatch ->
      {error, nomatch}
  end.

-spec is_pool_resource_table_registered(PoolRef :: atom()) -> boolean().
is_pool_resource_table_registered(PoolRef) ->
  PoolResourceTable = ?POOL_RESOURCE_TABLE(PoolRef),
  case ets:whereis(PoolResourceTable) of
    PoolResourceTable ->
      true;
    undefined ->
      false
  end.
