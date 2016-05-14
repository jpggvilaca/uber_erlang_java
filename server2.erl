-module(server2).
-author('Joao Vilaca').
-export([start/1]).

-define(TCP_OPTIONS, [list, {packet, 0}, {active, false}, {reuseaddr, true}]).

start(Port) ->
 {ok, ListenSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
 accept(ListenSocket).

accept(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  spawn(fun() -> loop(Socket) end),
  accept(ListenSocket).

loop(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      io:format("Received: ~s~n", [Data]),
      loop(Socket);
    {error, closed} ->
      ok
  end.