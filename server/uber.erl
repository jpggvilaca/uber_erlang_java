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
  TimerList = [],
  UserManager = spawn(fun() -> usermanager:userManager(UsersList) end),
  TripManager = spawn(fun() -> tripmanager:tripManager(DriversList, PassengersList, TripList, TimerList) end),
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
      io:format("Enviado register_ok para o user~n"),
      gen_tcp:send(Sock, "register_ok\n"),
      user(Sock);
    {register_failed} ->
      io:format("Enviado register_failed para o user~n"),
      gen_tcp:send(Sock, "register_failed\n"),
      user(Sock);
    {login_ok} ->
      io:format("Enviado login_ok para o user~n"),
      gen_tcp:send(Sock, "login_ok\n"),
      user(Sock);
    {login_failed} ->
      io:format("Enviado login_failed para o user~n"),
      gen_tcp:send(Sock, "login_failed\n"),
      user(Sock);
    {login_failed_user_already_exists} ->
      io:format("Enviado login_failed_user_already_exists para o user~n"),
      gen_tcp:send(Sock, "login_failed_user_already_exists\n"),
      user(Sock);
    {login_failed_wrong_password} ->
      io:format("Enviado login_failed_wrong_password para o user~n"),
      gen_tcp:send(Sock, "login_failed_wrong_password\n"),
      user(Sock);
    {login_failed_user_doesnt_exist} ->
      io:format("Enviado login_failed_user_doesnt_exist para o user~n"),
      gen_tcp:send(Sock, "login_failed_user_doesnt_exist\n"),
      user(Sock);

    % Check type of user
    {user_is_driver} ->
      gen_tcp:send(Sock, "user_is_driver\n"),
      user(Sock);
    {user_is_passenger} ->
      gen_tcp:send(Sock, "user_is_passenger\n"),
      user(Sock);

    % Trip
    {driver_added} ->
      io:format("Enviado driver_added para o user~n"),
      gen_tcp:send(Sock, "driver_added\n"),
      driver(Sock);
    {passenger_added} ->
      io:format("Enviado passenger_added para o user~n"),
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
      io:format("Enviado received_trip_request para o driver~n"),
      gen_tcp:send(Sock, "received_trip_request\n"),

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

      % Send the state to the tripmanager in case the passenger wants to cancel
      MomentOfRequest = now(),
      tripmanager ! {trip_state, Timer, MomentOfRequest, self(), Delay},

      % Loop
      driver(Sock);

    {trip_started} ->
      io:format("Enviado trip_started para o driver~n"),
      gen_tcp:send(Sock, "trip_started\n"),
      driver(Sock);

    {trip_ended} ->
      io:format("Enviado trip_ended para o driver~n"),
      gen_tcp:send(Sock, "trip_ended\n"),
      user(Sock);

    {cancel_trip, _} ->
      io:format("Enviado trip_ended para o driver~n"),
      gen_tcp:send(Sock, "trip_canceled\n"),
      user(Sock);
    {cancel_trip_before_time, _} ->
      io:format("Enviado cancel_trip_before_time para o driver~n"),
      gen_tcp:send(Sock, "cancel_trip_before_time\n"),
      user(Sock)
  end.



%% Handles passenger logic
passenger(Sock) ->
  receive
    {tcp, _, Data} ->
      tripmanager ! {tcp_response, self(), Data},
      passenger(Sock);
    {driver_arrived, _} ->
      io:format("Enviado driver_arrived para o passageiro~n"),
      gen_tcp:send(Sock, "driver_arrived\n"),
      passenger(Sock);
    {driver_info, Distance, Delay, Price, Model, Licence} ->
      Information =
        integer_to_list(Distance) ++ ":"
        ++ integer_to_list(Delay) ++ ":"
        ++ integer_to_list(Price) ++ ":"
        ++ Model ++ ":"
        ++ Licence,
      io:format("Enviado driver_info para o passageiro~n"),
      gen_tcp:send(Sock, "Info:" ++ Information ++ "\n"),
      passenger(Sock);
    {driver_available} ->
      io:format("Enviado driver_available para o passageiro~n"),
      gen_tcp:send(Sock, "driver_available\n"),
      passenger(Sock);
    {no_drivers_available} ->
      io:format("Enviado no_drivers_available para o passageiro~n"),
      gen_tcp:send(Sock, "no_drivers_available\n"),
      passenger(Sock);
    {trip_started} ->
      io:format("Enviado trip_started para o passageiro~n"),
      gen_tcp:send(Sock, "trip_started\n"),
      passenger(Sock);
    {cancel_trip, HasToPay} ->
      io:format("Enviado cancel_trip para o passageiro~n"),
      if
        HasToPay ->
          gen_tcp:send(Sock, "passenger_has_to_pay\n");
        true ->
          ok
      end,
      gen_tcp:send(Sock, "cancel_trip\n"),
      user(Sock);
    {cancel_trip_before_time, HasToPay} ->
      io:format("Enviado cancel_trip_before_time para o passageiro~n"),
      if
        HasToPay ->
          gen_tcp:send(Sock, "passenger_has_to_pay\n");
        true ->
          ok
      end,
      gen_tcp:send(Sock, "cancel_trip_before_time\n"),
      user(Sock);
    {trip_ended} ->
      io:format("Enviado trip_ended para o passageiro~n"),
      gen_tcp:send(Sock, "trip_ended\n"),
      user(Sock)
  end.