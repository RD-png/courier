%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%%
%%% @end
%%% Created :  13 Oct 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(courier_pool_SUITE).
-author("ryandenby").

-include_lib("eunit/include/eunit.hrl").

-export([all/0,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-export([test_start_all_SUITE/1, test_start_SUITE/1, test_stop_SUITE/1,
         test_restart_SUITE/1]).

-define(VALIDPOOL,      test3).
-define(UNDEFINED_POOL, undefined_pool).

all() ->
  [test_start_all_SUITE, test_start_SUITE, test_stop_SUITE,
   test_restart_SUITE].

init_per_suite(Config) ->
  application:ensure_all_started(courier),
  Config.

end_per_suite(_Config) ->
  application:stop(courier),
  ok.

init_per_testcase(_Name, Config) ->
  application:unset_env(courier, pools),
  Config.

end_per_testcase(_Name, _Config) ->
  ok.

%%%-------------------------------------------------------------------
%%% Test cases
%%%-------------------------------------------------------------------

test_start_all_SUITE(_Config) ->
  ExpectPoolsMissing = {error, pools_missing},
  ?assertEqual(ExpectPoolsMissing, courier_pool:start_all()),

  InvalidPools = [{test, []}],
  application:set_env(courier, pools, InvalidPools),

  ExpectInvalidOpts = {invalid_opts, {test, []}},
  ?assertException(throw, ExpectInvalidOpts, courier_pool:start_all()),

  ValidPools = [{test1, #{port => 8080, acceptors => 5, resources => []}},
                {test2, #{port => 8081, acceptors => 6, resources => []}}],
  application:set_env(courier, pools, ValidPools),
  ?assertEqual(ok, courier_pool:start_all()).

test_start_SUITE(_Config) ->
  ExpectPoolsMissing = {error, pools_missing},
  ?assertEqual(ExpectPoolsMissing, courier_pool:start(?UNDEFINED_POOL)),

  %% Set valid pools config
  Pools = [{?VALIDPOOL, #{port => 8083, acceptors => 5, resources => []}}],
  application:set_env(courier, pools, Pools),
  ct:log("Set pools config: ~p", [Pools]),

  ExpectUndefinedPool = {error, {undefined_pool_spec, test4}},
  ?assertEqual(ExpectUndefinedPool, courier_pool:start(test4)),

  ?assertEqual(ok, courier_pool:start(?VALIDPOOL)).

test_stop_SUITE(_Config) ->
  ?assertEqual({error, not_found}, courier_pool:stop(?UNDEFINED_POOL)),
  ?assertEqual({ok, test3},        courier_pool:stop(?VALIDPOOL)).

test_restart_SUITE(_Config) ->
  %% Attempt to restart a running pool
  ?assertEqual({error, running},   courier_pool:restart(test2)),
  ?assertEqual({error, not_found}, courier_pool:restart(?UNDEFINED_POOL)),
  ?assertEqual({ok, ?VALIDPOOL},   courier_pool:restart(?VALIDPOOL)).
