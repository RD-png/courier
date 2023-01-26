%%%-------------------------------------------------------------------
%%% @author Ryan User <ryan@nixos-desktop>
%%% @copyright (C) 2022, Ryan User
%%% @doc
%%% Courier HTTP request module
%%% @end
%%% Created : 11 Dec 2022 by Ryan User <ryan@nixos-desktop>
%%%-------------------------------------------------------------------
-module(courier_req).

-export([parse/1, parse_request_line/1, method/1, uri/1, version/1]).

-type req() :: #{Header :: binary() => Value :: term()}.
-export_type([req/0]).

%%%-------------------------------------------------------------------
%% API
%%%-------------------------------------------------------------------

-spec parse(RawReq) -> Req when
    RawReq :: binary(),
    Req :: req().
parse(RawReq) ->
  ReqList = do_parse(RawReq, []),
  maps:from_list(ReqList).

-spec parse_request_line(RawReq) -> ReqLineMap | Error when
    RawReq     :: binary(),
    ReqLineMap :: #{Key :: binary() => Value :: term()},
    Error      :: {error, request_format}.
parse_request_line(RawReq) ->
  case get_request_line(RawReq) of
    {Method, Uri, HTTPVer} ->
      #{
        <<"method">>  => Method,
        <<"uri">>     => Uri,
        <<"version">> => HTTPVer
       };
     {error, request_format} ->
      {error, request_format}
  end.

%% TODO
%% parse_query_string(RawReq) ->
%%   RawReq.

%% TODO
%% parse_body(RawReq) ->
%%   RawReq.

-spec method(RawReq) -> Value when
    RawReq :: binary(),
    Value  :: term().
method(RawReq) ->
  get(method, RawReq).

-spec uri(RawReq) -> Value when
    RawReq :: binary(),
    Value  :: term().
uri(RawReq) ->
  get(uri, RawReq).

-spec version(RawReq) -> Value when
    RawReq :: binary(),
    Value  :: term().
version(RawReq) ->
  get(version, RawReq).

%%%-------------------------------------------------------------------
%% Internal functions
%%%-------------------------------------------------------------------

get_request_line(RawReq) ->
  [ReqLine, _Headers] = binary:split(RawReq, <<"\r\n">>),
  case binary:split(ReqLine, <<$ :8>>, [global]) of
    [_Method, _Uri, _HTTPVer] = ReqLineSegments ->
      list_to_tuple(ReqLineSegments);
    _ ->
      {error, request_format}
  end.

get(Field, RawReq) ->
  ReqLine = get_request_line(RawReq),
  do_get(Field, ReqLine).

do_get(method, RequestLine) ->
  element(1, RequestLine);
do_get(uri, RequestLine) ->
  element(2, RequestLine);
do_get(version, RequestLine) ->
  element(2, RequestLine).

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
