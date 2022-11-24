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
-export([]).

-type resource() :: {UriName     :: binary(),
                     Uri         :: iodata(),
                     Handler     :: module(),
                     HandlerArgs :: [term()]}.
-type resources() :: [resource()].
-export_type([resources/0]).

-type uri_pattern() :: {UriPattern     :: re:mp(),
                        UriPatternKeys ::{namelist, [binary()]}}.

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

create_uri_pattern(UriRegex) ->
  {ok, UriPattern}           = re:compile(UriRegex),
  {namelist, UriPatternKeys} = re:inspect(UriPattern, namelist),
  {UriPattern, UriPatternKeys}.

get_uri_variable_map({UriPattern, UriPatternKeys}) ->
  {match, UriVars} = re:run(UriPattern, [global, trim_all]),
  maps:from_list(lists:zip(UriPatternKeys, UriVars)).
