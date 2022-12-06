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

-export([fetch_resource/1, fetch_all_resources/1, add_resource/2,
         add_resources/2, delete_resource/1, update_resource/1,
         update_resource/2]).

-record resource_spec, {uri     :: atom(),
                        pool    :: atom(),
                        spec    :: uri_spec(),
                        handler :: module(),
                        args    :: term()}.

-type resource_spec() :: #resource_spec{}.
-type uri_spec() :: {UriPattern     :: term(), UriPatternKeys :: [binary()]}.
-type resource() :: {UriRef      :: atom(),
                     UriRegex    :: iodata(),
                     Handler     :: module(),
                     HandlerArgs :: term()}.
-export_type([resource/0]).


-define(TABLE, pool_resources).
-define(is_resource_spec(Arg), element(1, Arg) =:= resource_spec).

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
      ets:new(?TABLE, [set, public, named_table, {keypos, #resource_spec.uri},
                       {read_concurrency, true}, {write_concurrency, true}]),
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
-spec fetch_all_resources(PoolRef :: atom()) ->
        [resource()] | {error, resource_undefined}.
fetch_all_resources(PoolRef) ->
  Matches = ets:select(?TABLE, [{{resource_spec, '_', '$1', '_', '_', '_'},
                                 [{'=:=', '$1', PoolRef}],
                                 ['$_']}]),
  case Matches of
    [] ->
      {error, resource_undefined};
    Resources ->
      Resources
  end.

-spec fetch_resource(UriRef :: atom()) ->
        resource() | {error, resource_undefined}.
fetch_resource(UriRef) ->
  case ets:lookup(?TABLE, UriRef) of
    [Resource] ->
      Resource;
    [] ->
      {error, resource_undefined}
    end.

-spec add_resource(PoolRef :: atom(), Resource :: resource()) ->
        ok | {error, invalid_uri_regex | resource_exists}.
add_resource(PoolRef, {UriRef, UriRegex, Handler, HandlerArgs} = _Resource) ->
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

-spec add_resources(PoolRef :: atom(), Resources :: [resource()]) -> ok.
add_resources(PoolRef, Resources) ->
  lists:foreach(fun(Resource) ->
                    ok = add_resource(PoolRef, Resource)
                end, Resources).

-spec delete_resource(UriRef :: atom()) -> ok | {error, resource_undefined}.
delete_resource(UriRef) ->
  case ets:lookup(?TABLE, UriRef) of
    [_Resource] ->
      ets:delete(?TABLE, UriRef),
      ok;
    [] ->
      {error, resource_undefined}
  end.

-spec update_resource(UpdatedResource :: resource_spec()) ->
        true | {error, resource_undefined}.
update_resource(UpdatedResource) when ?is_resource_spec(UpdatedResource) ->
  case ets:lookup(?TABLE, UpdatedResource#resource_spec.uri) of
    [_Resource] ->
      ets:insert(?TABLE, UpdatedResource);
    [] ->
      {error, resource_undefined}
  end.

-spec update_resource(UriRef :: atom(), {Field :: atom(),
                                         UpdatedValue :: term()}) ->
        boolean() | {error, resource_undefined | resource_field_invalid}.
update_resource(UriRef, {Field, UpdatedValue}) when is_atom(Field) ->
  case ets:lookup(?TABLE, UriRef) of
    [ResourceSpec] ->
      case update_resource_spec_field(Field, UpdatedValue, ResourceSpec) of
        {error, resource_field_invalid} = Err ->
          Err;
        UpdatedResourceSpec ->
          ets:insert(?TABLE, UpdatedResourceSpec)
      end;
    [] ->
      {error, resource_undefined}
  end.

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
