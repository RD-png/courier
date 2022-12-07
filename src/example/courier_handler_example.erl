-module(courier_handler_example).

-export([execute/2]).

execute(_UriVarMap, _HandlerArgs) ->
  "HTTP/1.1 200 OK\r\nContent-Length: 13\r\nConnection: close\r\n\r\nHello, world!".
