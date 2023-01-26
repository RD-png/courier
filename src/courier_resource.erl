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

-export([fetch/1, match/2, pool_fetch_all_resources/1, new/2, new_multi/2,
         delete/1, update/1, update/3]).

-record resource_spec, {uri     :: atom(),
                        pool    :: atom(),
                        spec    :: uri_spec(),
                        handler :: module(),
                        args    :: term()}.

-type resource_spec() :: #resource_spec{}.

-type uri_spec() :: {UriPattern :: term(), UriPatternKeys :: [binary()]}.

-type uri_var_map() :: #{UriPatternKey :: binary() => UriVar :: term()}.
-export_type([uri_var_map/0]).

-type resource() :: {UriRef      :: atom(),
                     UriRegex    :: iodata(),
                     Handler     :: module(),
                     HandlerArgs :: term()}.
-export_type([resource/0]).


-define(RESOURCE_ETS, pool_resources).

-define(is_resource_spec(Arg), element(1, Arg) =:= resource_spec).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Create an ets table to store the resoruces for acceptor pools.
-spec init_resource_table() -> Tid | table_already_registered when
    Tid :: ets:tid().
init_resource_table() ->
  case is_pool_resource_table_registered() of
    true ->
      table_already_exists;
    false ->
      ets:new(?RESOURCE_ETS, [set, public, named_table,
                              {keypos, #resource_spec.uri},
                              {read_concurrency, true},
                              {write_concurrency, true}])
  end.

%% @doc Delete a resource table for a registered pool.
-spec delete_resource_table() -> true | table_not_registered.
delete_resource_table() ->
  case is_pool_resource_table_registered() of
    true ->
      ets:delete(?RESOURCE_ETS);
    false ->
      table_not_registered
  end.

%% @doc Fetch a list of all resources for `PoolRef'
-spec pool_fetch_all_resources(PoolRef) -> Resources when
    PoolRef   :: atom(),
    Resources :: [resource_spec()].
pool_fetch_all_resources(PoolRef) ->
  ets:select(?RESOURCE_ETS, [{{resource_spec, '_', '$1', '_', '_', '_'},
                                        [{'=:=', '$1', PoolRef}],
                                        ['$_']}]).

%% @doc Fetch a single resource for a given `UriRef'
-spec fetch(UriRef) -> Resource | {error, Reason} when
    UriRef   :: atom(),
    Resource :: resource(),
    Reason   :: resource_undefined.
fetch(UriRef) ->
  case ets:lookup(?RESOURCE_ETS, UriRef) of
    [Resource] ->
      Resource;
    [] ->
      {error, resource_undefined}
    end.

-spec match(PoolRef, Uri) -> {ok, Match} | nomatch when
    PoolRef     :: atom(),
    Uri         :: iodata(),
    Match       :: {UriVarMap, Handler, HandlerArgs},
    UriVarMap   :: uri_var_map(),
    Handler     :: module(),
    HandlerArgs :: term().
match(PoolRef, Uri) ->
  Resources = pool_fetch_all_resources(PoolRef),
  do_match(Uri, Resources).

%% @doc Insert a new `Resource' for `PoolRef'
-spec new(PoolRef, Resource) -> ok | {error, Reason} when
    PoolRef  :: atom(),
    Resource :: resource(),
    Reason   :: invalid_uri_regex | resource_exists.
new(PoolRef, {UriRef, UriRegex, Handler, HandlerArgs} = _Resource) ->
  case create_uri_spec(UriRegex) of
    {error, invalid_uri_regex} = Err ->
      Err;
    {_UriPattern, _UriPatternKeys} = UriSpec ->
      try_add_resource(#resource_spec{uri     = UriRef,
                                      pool    = PoolRef,
                                      spec    = UriSpec,
                                      handler = Handler,
                                      args    = HandlerArgs})
  end.

%% @doc Insert multiple new `Resources' for `PoolRef'
-spec new_multi(PoolRef, Resources) -> ok when
    PoolRef   :: atom(),
    Resources :: [resource()].
new_multi(PoolRef, Resources) when is_list(Resources)->
  lists:foreach(fun(Resource) ->
                    ok = new(PoolRef, Resource)
                end, Resources).

%% @doc Delete a resource for a pool given its unique identifier `UriRef'
-spec delete(UriRef) -> ok when
    UriRef :: atom().
delete(UriRef) ->
  ets:delete(?RESOURCE_ETS, UriRef),
  ok.

%% @doc Update an entire resource with `UpdatedResource'
-spec update(UpdatedResource) -> true | {error, Reason} when
    UpdatedResource :: resource_spec(),
    Reason          :: resource_undefined.
update(UpdatedResource) when ?is_resource_spec(UpdatedResource) ->
  case ets:member(?RESOURCE_ETS, UpdatedResource#resource_spec.uri) of
    true ->
      ets:insert(?RESOURCE_ETS, UpdatedResource);
    false ->
      {error, resource_undefined}
  end.

%% @doc Update a `Field' value with the `UpdatedValue' for a given
%% unique identifier `UriRef'
-spec update(UriRef, Field, UpdatedValue) -> boolean() | {error, Reason} when
    UriRef       :: atom(),
    Field        :: atom(),
    UpdatedValue :: term(),
    Reason       :: resource_undefined | resource_field_invalid.
update(UriRef, Field, UpdatedValue) when is_atom(Field) ->
  case ets:lookup(?RESOURCE_ETS, UriRef) of
    [ResourceSpec] ->
      case update_resource_spec_field(Field, UpdatedValue, ResourceSpec) of
        {error, resource_field_invalid} = Err ->
          Err;
        UpdatedResourceSpec ->
          ets:insert(?RESOURCE_ETS, UpdatedResourceSpec)
      end;
    [] ->
      {error, resource_undefined}
  end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

create_uri_spec(UriRegex) ->
  case re:compile(UriRegex, []) of
    {ok, UriPattern} ->
      {namelist, UriPatternKeys} = re:inspect(UriPattern, namelist),
      {UriPattern, UriPatternKeys};
    {error, _Reason} ->
      {error, invalid_uri_regex}
  end.

is_pool_resource_table_registered() ->
  case ets:whereis(?RESOURCE_ETS) of
    undefined ->
      false;
    _Tid ->
      true
  end.

try_add_resource(ResourceSpec) ->
  case ets:insert_new(?RESOURCE_ETS, ResourceSpec) of
    true ->
      ok;
    false ->
      {error, resource_exists}
  end.

do_match(_Uri, []) ->
  nomatch;
do_match(Uri, [#resource_spec{spec = {UriPattern, UriPatternKeys},
                              handler = Handler, args = HandlerArgs}
              | Rest]) ->
  case re:run(Uri, UriPattern, [notempty, {capture, all_names, binary}]) of
    {match, UriVars} when length(UriVars) == length(UriPatternKeys) ->
      {ok, {get_uri_var_map(UriPatternKeys, UriVars), Handler, HandlerArgs}};
    match when length(UriPatternKeys) == 0 ->
      {ok, {#{}, Handler, HandlerArgs}};
    _NoMatch ->
      do_match(Uri, Rest)
  end.

get_uri_var_map(UriPatternKeys, UriVars) ->
  maps:from_list(lists:zip(UriPatternKeys, UriVars)).

update_resource_spec_field(pool,    Value, #resource_spec{} = RS) ->
  RS#resource_spec{pool = Value};
update_resource_spec_field(spec,    Value, #resource_spec{} = RS) ->
  RS#resource_spec{spec = Value};
update_resource_spec_field(handler, Value, #resource_spec{} = RS) ->
  RS#resource_spec{handler = Value};
update_resource_spec_field(args,    Value, #resource_spec{} = RS) ->
  RS#resource_spec{args = Value};
update_resource_spec_field(_, _, _) ->
  {error, resource_field_invalid}.
