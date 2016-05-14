-module(uber).
-export([start/1]).
-author("João Vilaça").
-define(TCP_OPTIONS, [list, {packet, 0}, {reuseaddr, true}]).

%% Create the socket connection, initiate all processes and data
start(Port) ->
  UsersList = [],
  UserManager = spawn(fun() -> usermanager:userManager(UsersList) end),
  TripManager = spawn(fun() -> tripmanager:tripManager() end),
  register(usermanager, UserManager),
  register(loginmanager, spawn(fun() -> loginmanager:loginManager() end)),
  register(tripmanager, TripManager),
  {ok, LSock} = gen_tcp:listen(Port, ?TCP_OPTIONS),
  acceptor(LSock, UserManager, TripManager).

%% Accept socket connections, becomes a user and spawns another acceptor
acceptor(LSock, UserManager, TripManager) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  spawn(fun() -> acceptor(LSock, UserManager, TripManager) end),
  UserManager ! {enter, self()},
  user(Sock, UserManager, TripManager).

%% User instance, direct connection with the client
user(Sock, UserManager, TripManager) ->
  receive
    {tcp, _, Data} ->
      UserManager ! {tcp_response, self(), Data},
      user(Sock, UserManager, TripManager);
    {register_ok} ->
      gen_tcp:send(Sock, "register_ok\n"),
      user(Sock, UserManager, TripManager);
    {login_ok} ->
      gen_tcp:send(Sock, "login_ok\n"),
      user(Sock, UserManager, TripManager);
    {login_failed} ->
      gen_tcp:send(Sock, "login_failed\n"),
      user(Sock, UserManager, TripManager);
    {driver_ready} ->
      gen_tcp:send(Sock, "driver_ready\n"),
      driver(Sock, TripManager);
    {driver_error} ->
      gen_tcp:send(Sock, "driver_error\n"),
      user(Sock, UserManager, TripManager);
    {tcp_closed, _} ->
      UserManager ! {leave, self()};
    {tcp_error, _, _} ->
      UserManager ! {leave, self()}
  end.

driver(Sock, TripManager) ->
  receive
    {tcp, _, Data} ->
      TripManager ! {request, self(), Data},
      driver(Sock, TripManager);
    {want_trip} ->
      gen_tcp:send(Sock, "want_trip_ok\n"),
      driver(Sock, TripManager);
    {can_drive} ->
      gen_tcp:send(Sock, "can_drive_ok\n"),
      driver(Sock, TripManager);
    {tcp_closed, _} ->
      TripManager ! {leave, self()};
    {tcp_error, _, _} ->
      TripManager ! {leave, self()}
  end.