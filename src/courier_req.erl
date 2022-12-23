%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%% Courier HTTP request module
%%% @end
%%% Created :  11 Dec 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(courier_req).

-export([parse/1, get/2]).

-type req() :: #{Header :: binary() => Value :: term()}.
-export_type([req/0]).

%%%-------------------------------------------------------------------
%% API
%%%-------------------------------------------------------------------

-spec parse(RawReq :: binary()) -> req().
parse(RawReq) ->
  ReqList = do_parse(RawReq, []),
  maps:from_list(ReqList).

-spec get(Field :: atom(), RawReq :: binary()) -> FieldValue :: term().
get(uri, RawReq) ->
  [UriSegment, _Rest] = binary:split(RawReq, <<"\r\n">>),
  [_Method, Uri, _HTTPVer] = binary:split(UriSegment, <<" ">>, [global]),
  Uri.

%%%-------------------------------------------------------------------
%% Internal functions
%%%-------------------------------------------------------------------

do_parse(<<>>, Parsed) ->
  Parsed;
do_parse(<<Header, Headers/binary>>, Parsed) ->
  ParsedHeader = parse_header(Header),
  do_parse(Headers, [ParsedHeader | Parsed]).

parse_header(Header) ->
  parse_header(Header, <<>>).

parse_header(<<>>, ParsedHeader) ->
  ParsedHeader;
parse_header(<<$:, HeaderValue/binary>>, ParsedHeader) ->
  ParsedHeaderValue = parse_header_value(HeaderValue),
  {ParsedHeader, ParsedHeaderValue};
parse_header(<<Bit, Rest/binary>>, ParsedHeader) ->
  %% Build a binary header string.
  parse_header(Rest, [Bit | ParsedHeader]).

parse_header_value(HeaderValue) ->
  parse_header_value(HeaderValue, <<>>).

parse_header_value(<<>>, ParsedHeaderValue) ->
  ParsedHeaderValue;
parse_header_value(<<$\r, $\n, _Rest>>, ParsedHeaderValue) ->
  ParsedHeaderValue;
 parse_header_value(<<Bit, Rest/binary>>, ParsedHeaderValue) ->
  parse_header_value(Rest, [Bit | ParsedHeaderValue]).
