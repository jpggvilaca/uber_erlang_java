-module(uber).
-export([start/1]).
-author("João Vilaça").
-define(TCP_OPTIONS, [list, {packet, 0}, {reuseaddr, true}]).

%% Create the socket connection, initiate all processes and data
start(Port) ->
  UsersList = [],
  DriversList = [],
  PassengersList = [],
  UserManager = spawn(fun() -> usermanager:userManager(UsersList, DriversList, PassengersList) end),
  TripManager = spawn(fun() -> tripmanager:tripManager() end),
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
    {driver_added, NewDriver} ->
      gen_tcp:send(Sock, "driver_added\n"),
      driver(NewDriver);
    % {driver_info} ->
    %   gen_tcp:send(Sock, "driver_info\n"),
    %   user(Sock);
    {driver_error} ->
      gen_tcp:send(Sock, "driver_error\n"),
      user(Sock);
    {passenger_added, Passenger, DriversList} ->
      gen_tcp:send(Sock, "passenger_added\n"),
      passenger(Passenger, DriversList);

    % Error/Disconnect
    {tcp_closed, _} ->
      usermanager ! {leave, self()},
      user(Sock);
    {tcp_error, _, _} ->
      usermanager ! {leave, self()},
      user(Sock)
  end.

%% Handles driver logic
driver(Driver) ->
  io:format("sou driver~n"),

  receive
    % Data should have the passenger pid and X1,Y1,X2,Y2
    {trip_request, PassengerPid, FromX, FromY} ->
      % Parse Data
      {DPid, DX, DY,M,L} = Driver,

      io:format("~nPid do driver:"),
      aux:debug(DPid),

      io:format("~nPid do passageiro:"),
      aux:debug(PassengerPid),


      % Calculate delay
      Distance = aux:distance(DX, DY, FromX, FromY),
      Delay = aux:time(Distance),
      Price = aux:price(Distance),
      Model = M,
      Licence = L,

      % Send message to tell user the Trip info (cost, distance, etc...)
      PassengerPid ! {driver_info, Distance, Delay, Price, Model, Licence},
      usermanager ! {driver_info, DPid},

      % Send message to passenger warning the arrival (set 2000 to 1000)
      timer:send_after(Delay*2000, PassengerPid, {driver_arrived, Driver}),

      % Loop
      driver(Driver);

    {cancel_request, Data} ->
      io:format("viagem cancelada"),
      driver(Driver)
  end.

%% Handles passenger logic
passenger(Passenger, DriversList) ->
  % [H|T] = DriversList,
  % {Pid, _, _,_,_} = H,
  % {_, FromX, FromY, _, _} = Passenger,
  % Pid ! {trip_request, self(), FromX, FromY},
  io:format("sou passenger!"),
  % aux:debug(Pid),

  receive
    % Data should have the driver pid and X, Y
    {driver_arrived, Driver} ->
      io:format("driver chegou!!~n"),
      io:format("Driver: ~p", [Driver]),
      passenger(Passenger, DriversList);
    {driver_info, Distance, Delay, Price, Model, Licence} ->
      io:format("informação do driver chegou!!~n"),
      passenger(Passenger, DriversList)
      % cancel or enter car

  end.


% trip(Passenger, Driver) ->
%   receive
%     {start} ->