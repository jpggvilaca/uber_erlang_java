-module(uber).
-export([start/1]).
-author("João Vilaça").
-define(TCP_OPTIONS, [list, {packet, 0}, {reuseaddr, true}]).

%% Create the socket connection, initiate all processes and data
start(Port) ->
  UsersList = [],
  DriversList = [],
  PassengersList = [],
  UserManager = spawn(fun() -> usermanager:userManager(UsersList, DriversList) end),
  TripManager = spawn(fun() -> tripmanager:tripManager(DriversList, PassengersList) end),
  register(usermanager, UserManager),
  register(loginmanager, spawn(fun() -> loginmanager:loginManager() end)),
  register(tripmanager, TripManager),
  {ok, LSock} = gen_tcp:listen(Port, ?TCP_OPTIONS),
  acceptor(LSock).

%% Accept socket connections, spawns another acceptor and executes user (becomes user)
acceptor(LSock) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  spawn(fun() -> acceptor(LSock) end),
  usermanager ! {enter, self()},
  user(Sock).

%% User instance, direct connection with the client
user(Sock) ->
  receive % From tcp
    {tcp, _, Data} ->
      usermanager ! {tcp_response, self(), Data}, % Send tcp to UserManager
      user(Sock);

    % Login / Register
    {register_ok} ->
      gen_tcp:send(Sock, "register_ok\n"),
      user(Sock);
    {register_failed} ->
      gen_tcp:send(Sock, "register_failed\n"),
      user(Sock);
    {login_ok} ->
      gen_tcp:send(Sock, "login_ok\n"),
      user(Sock);
    {login_failed} ->
      gen_tcp:send(Sock, "login_failed\n"),
      user(Sock);

    % Trip
    {driver_available} ->
      gen_tcp:send(Sock, "driver_available\n"),
      user(Sock);
    {driver_arrived} ->
      gen_tcp:send(Sock, "driver_arrived\n"),
      user(Sock);
    {driver_added} ->
      gen_tcp:send(Sock, "driver_added\n"),
      user(Sock);
    {driver_info} ->
      gen_tcp:send(Sock, "driver_info\n"),
      user(Sock);
    {driver_error} ->
      gen_tcp:send(Sock, "driver_error\n"),
      user(Sock);
    {passenger_added} ->
      gen_tcp:send(Sock, "passenger_added\n"),
      user(Sock);

    % Error/Disconnect
    {tcp_closed, _} ->
      usermanager ! {leave, self()},
      user(Sock);
    {tcp_error, _, _} ->
      usermanager ! {leave, self()},
      user(Sock)
  end.