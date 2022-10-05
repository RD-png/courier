%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%% Courier app.
%%% @end
%%% Created :  5 Oct 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------

-module(courier_app).

-behaviour(application).

%% API
-export([start/2,
         stop/1,
         get_env/1,
         get_env_or_default/2]).

-define(APP, courier).

%%%-------------------------------------------------------------------
%% API
%%%-------------------------------------------------------------------

start(_StartType, _StartArgs) ->
  courier_sup:start_link().

stop(_State) ->
  ok.

%% @doc Return value bound to key in app environemnt, `Key' is expected
%%      to be set, so we will throw an error otherwise.
-spec get_env(Key) -> Value | no_return() when
  Key   :: term(),
  Value :: term().
get_env(Key) when is_atom(Key) ->
  case application:get_env(Key) of
    {ok, Val} ->
      Val;
    undefined ->
      throw({env_missing_key, Key})
  end.

%% @doc Return value bound to key in app environemnt, `Key' maybe not be
%%      set so return the passed `Default' otherwise.
-spec get_env_or_default(Key, Default) -> Value when
  Key     :: term(),
  Default :: term(),
  Value   :: term().
get_env_or_default(Key, Default) ->
  application:get_env(?APP, Key, Default).
