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
  acceptor(LSock, UserManager, TripManager).

%% Accept socket connections, spawns another acceptor and executes user (becomes user)
acceptor(LSock, UserManager, TripManager) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  spawn(fun() -> acceptor(LSock, UserManager, TripManager) end),
  UserManager ! {enter, self()},
  user(Sock, UserManager, TripManager).

%% User instance, direct connection with the client
user(Sock, UserManager, TripManager) ->
  receive
    %% TCP
    {tcp, _, Data} ->
      UserManager ! {tcp_response, self(), Data}, % Send tcp to UserManager
      user(Sock, UserManager, TripManager);

    %% LOGIN / REGISTER
    {register_ok} ->
      gen_tcp:send(Sock, "register_ok\n"),
      user(Sock, UserManager, TripManager);
    % {register_failed} ->
    %   gen_tcp:send(Sock, "register_failed\n"),
    %   user(Sock, UserManager, TripManager);
    {login_ok} ->
      gen_tcp:send(Sock, "login_ok\n"),
      user(Sock, UserManager, TripManager);
    {login_failed} ->
      gen_tcp:send(Sock, "login_failed\n"),
      user(Sock, UserManager, TripManager);


    %% TRIP
    {driver_available} ->
      gen_tcp:send(Sock, "driver_available\n"),
      user(Sock, UserManager,TripManager);
    {driver_arrived} ->
      gen_tcp:send(Sock, "driver_arrived\n"),
      user(Sock, UserManager,TripManager);
    {driver_added} ->
      gen_tcp:send(Sock, "driver_added\n"),
      user(Sock, UserManager,TripManager);
    {driver_error} ->
      gen_tcp:send(Sock, "driver_error\n"),
      user(Sock, UserManager,TripManager);
    {passenger_added} ->
      gen_tcp:send(Sock, "passenger_added\n"),
      user(Sock, UserManager,TripManager);


    {tcp_closed, _} ->
      UserManager ! {leave, self()},
      user(Sock, UserManager, TripManager);
    {tcp_error, _, _} ->
      UserManager ! {leave, self()},
      user(Sock, UserManager, TripManager)
  end.