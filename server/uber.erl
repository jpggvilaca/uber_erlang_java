-module(uber).
-export([start/1]).
-author("João Vilaça").
-define(TCP_OPTIONS, [list, {packet, 0}, {reuseaddr, true}]).

%% Create the socket connection, initiate all processes and data
start(Port) ->
  UsersList = [],
  DriversList = [],
  PassengersList = [],
  TripList = [], % Passenger-Driver tuples
  UserManager = spawn(fun() -> usermanager:userManager(UsersList) end),
  TripManager = spawn(fun() -> tripmanager:tripManager(DriversList, PassengersList, TripList) end),
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
    {driver_arrived} ->
      gen_tcp:send(Sock, "driver_arrived\n"),
      user(Sock);
    {driver_added} ->
      gen_tcp:send(Sock, "driver_added\n"),
      driver(Sock);
    {passenger_added} ->
      gen_tcp:send(Sock, "driver_added\n"),
      passenger(Sock);

    % Error/Disconnect
    {tcp_closed, _} ->
      usermanager ! {leave, self()},
      user(Sock);
    {tcp_error, _, _} ->
      usermanager ! {leave, self()},
      user(Sock)
  end.

%% Handles driver logic
driver(Sock) ->
  receive
    {trip_request, Driver, Passenger, FromX, FromY} ->
      % Parse Data
      {_, DX, DY,M,L} = Driver,
      {PPid, FromX, FromY, _, _} = Passenger,

      % Calculate trip info
      Distance = aux:distance(DX, DY, FromX, FromY),
      Delay = aux:time(Distance),
      Price = aux:price(Distance),
      Model = M,
      Licence = L,

      % Send message to tell user the Trip info (cost, distance, etc...)
      PPid ! {driver_info, Distance, Delay, Price, Model, Licence},

      % Send message to passenger warning the arrival (set 2000 to 1000)
      timer:send_after(Delay*1000, PPid, {driver_arrived, Driver}),

      % Loop
      driver(Sock);

    {trip_ended} ->
      io:format("Viagem chegou ao fim");

    {cancel_request, _} ->
      io:format("viagem cancelada"),
      driver(Sock)
  end.

%% Handles passenger logic
passenger(Sock) ->
  % if
  %   length(DriversList) > 0 ->
  %     % Fetch the X and Y from the passenger's current location
  %     [H|T] = DriversList,
  %     {Pid, _, _,_,_} = H,
  %     {_, FromX, FromY, _, _} = Passenger,

  %     % Send a trip_request to the driver
  %     Pid ! {trip_request, self(), FromX, FromY},
  %     [DriversList -- [H]];
  %   true ->
  %     io:format("DriversList vazia")
  % end,
  receive
    {tcp, _, Data} ->
      tripmanager ! {tcp_response, self(), Data}, % Send tcp to TripManager
      passenger(Sock);
    {driver_arrived, Driver} ->
      io:format("driver chegou!!~n"),
      io:format("Driver: ~p", [Driver]),
      passenger(Sock);
    {driver_info, Distance, Delay, Price, Model, Licence} ->
      io:format("informação do driver chegou!!~n"),
      passenger(Sock);
    {trip_ended} ->
      io:format("Viagem chegou ao fim")
      % cancel or enter car
  end.