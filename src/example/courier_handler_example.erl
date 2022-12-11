-module(courier_handler_example).
-behaviour(courier_handler).

-export([handle/3]).

handle(_Req, _UriVarMap, _HandlerArgs) ->
  {ok, <<"HTTP/1.1 200 OK\r\nContent-Length: 13\r\nConnection: close\r\n\r\nHello, world!">>}.
