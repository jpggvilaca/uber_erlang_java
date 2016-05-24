-module(uber).
-export([start/1]).
-author("João Vilaça").
-define(TCP_OPTIONS, [list, {packet, 0}, {reuseaddr, true}]).

%% Create the socket connection, initiate all processes and data
start(Port) ->
  UsersList = [],
  DriversList = [],
  PassengersList = [],
  UserManager = spawn(fun() -> usermanager:userManager(UsersList) end),
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
    {driver_added, NewDriver, PassengersList} ->
      gen_tcp:send(Sock, "driver_added\n"),
      driver(NewDriver, PassengersList);
    {passenger_added, Passenger, DriversList} ->
      gen_tcp:send(Sock, "passenger_added\n"),
      passenger(Passenger, DriversList);
    % {driver_info, DriverPid} ->
    %   gen_tcp:send(Sock, "driver_info\n"),
    %   user(Sock);
    % {driver_error} ->
    %   gen_tcp:send(Sock, "driver_error\n"),
    %   user(Sock);

    % Error/Disconnect
    {tcp_closed, _} ->
      usermanager ! {leave, self()},
      user(Sock);
    {tcp_error, _, _} ->
      usermanager ! {leave, self()},
      user(Sock)
  end.

%% Handles driver logic
driver(Driver, PassengersList) ->
  io:format("sou driver~n"),

  receive
    {trip_request, PassengerPid, FromX, FromY} ->
      % Parse Data
      {DPid, DX, DY,M,L} = Driver,
      [H|T] = PassengersList,
      Passenger = H,

      % Calculate delay
      Distance = aux:distance(DX, DY, FromX, FromY),
      Delay = aux:time(Distance),
      Price = aux:price(Distance),
      Model = M,
      Licence = L,

      % Send message to tell user the Trip info (cost, distance, etc...)
      PassengerPid ! {driver_info, Distance, Delay, Price, Model, Licence},
      % usermanager ! {driver_info, DPid},

      % Send message to passenger warning the arrival (set 2000 to 1000)
      timer:send_after(Delay*2000, PassengerPid, {driver_arrived, Driver}),

      % Loop
      driver(Driver, PassengersList);

    {cancel_request, Data} ->
      io:format("viagem cancelada"),
      driver(Driver, PassengersList)
  end.

%% Handles passenger logic
passenger(Passenger, DriversList) ->
  if
    length(DriversList) > 0 ->
      % Fetch the X and Y from the passenger's current location
      [H|T] = DriversList,
      {Pid, _, _,_,_} = H,
      {_, FromX, FromY, _, _} = Passenger,

      % Send a trip_request to the driver
      Pid ! {trip_request, self(), FromX, FromY},
      [DriversList -- [H]];
    true ->
      io:format("DriversList vazia")
  end,

  receive
    {driver_arrived, Driver} ->
      io:format("driver chegou!!~n"),
      io:format("Driver: ~p", [Driver]),
      passenger(Passenger, DriversList);
    {driver_info, Distance, Delay, Price, Model, Licence} ->
      io:format("informação do driver chegou!!~n"),
      passenger(Passenger, DriversList)
      % cancel or enter car
  end.