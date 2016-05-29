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
  Timer = {result, value},
  UserManager = spawn(fun() -> usermanager:userManager(UsersList) end),
  TripManager = spawn(fun() -> tripmanager:tripManager(DriversList, PassengersList, TripList, Timer) end),
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

%% User instance, direct connection with the socket (client)
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
    {login_failed_user_already_exists} ->
      gen_tcp:send(Sock, "login_failed_user_already_exists\n"),
      user(Sock);
    {login_failed_wrong_password} ->
      gen_tcp:send(Sock, "login_failed_wrong_password\n"),
      user(Sock);
    {login_failed_user_doesnt_exist} ->
      gen_tcp:send(Sock, "login_failed_user_doesnt_exist\n"),
      user(Sock);

    % Trip
    {driver_added} ->
      gen_tcp:send(Sock, "driver_added\n"),
      driver(Sock);
    {passenger_added} ->
      gen_tcp:send(Sock, "passenger_added\n"),
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

      % Send message to tell user the Trip info
      PPid ! {driver_info, Distance, Delay, Price, Model, Licence},

      % Send message to passenger warning the arrival
      Timer = timer:send_after(Delay*1000, PPid, {driver_arrived, Driver}),

      % Send the timer to the tripmanager in case the passenger wants to cancel
      tripmanager ! {cancel_trip_before_time, Timer},

      % Loop
      driver(Sock);

    {trip_started} ->
      io:format("Viagem começou!~n"),
      gen_tcp:send(Sock, "trip_started\n"),
      driver(Sock);

    {trip_ended} ->
      gen_tcp:send(Sock, "trip_ended\n"),
      io:format("Viagem chegou ao fim (Condutor)~n"),
      driver(Sock);

    {cancel_request, _} ->
      io:format("Viagem cancelada (condutor)"),
      gen_tcp:send(Sock, "trip_canceled\n"),
      driver(Sock);
    {cancel_request_before_time, _} ->
      io:format("Viagem cancelada antes do tempo (condutor)"),
      gen_tcp:send(Sock, "cancel_request_before_time\n"),
      driver(Sock)
  end.



%% Handles passenger logic
passenger(Sock) ->
  receive
    {tcp, _, Data} ->
      tripmanager ! {tcp_response, self(), Data},
      passenger(Sock);
    {driver_arrived, Driver} ->
      io:format("Driver ~p chegou até ao passageiro!~n", [Driver]),
      gen_tcp:send(Sock, "driver_arrived\n"),
      passenger(Sock);
    {driver_info, Distance, Delay, Price, Model, Licence} ->
      io:format("Driver info chegou ao passageiro~n"),
      Information =
        integer_to_list(Distance) ++ ":"
        ++ integer_to_list(Delay) ++ ":"
        ++ integer_to_list(Price) ++ ":"
        ++ Model ++ ":"
        ++ Licence,
      gen_tcp:send(Sock, "Info:" ++ Information ++ "\n"),
      passenger(Sock);
    {driver_available} ->
      io:format("Já há condutores disponíveis~n"),
      gen_tcp:send(Sock, "driver_available\n"),
      passenger(Sock);
    {no_drivers_available} ->
      io:format("Não há condutores disponíveis~n"),
      gen_tcp:send(Sock, "no_drivers_available\n"),
      passenger(Sock);
    {trip_started} ->
      io:format("Viagem começou!~n"),
      gen_tcp:send(Sock, "trip_started\n"),
      passenger(Sock);
    {cancel_request_before_time} ->
      io:format("Viagem cancelada antes do tempo (passageiro)~n"),
      gen_tcp:send(Sock, "cancel_request_before_time\n"),
      passenger(Sock);
    {trip_ended} ->
      gen_tcp:send(Sock, "trip_ended\n"),
      io:format("Viagem chegou ao fim (passageiro)~n"),
      passenger(Sock)
  end.