-module(handletripmanager).
-export([handleTripManager/2]).

handleTripManager(DriversList, PassengersList) ->
  receive
    % {driver_arrived, Pid} -> ISTO AQUI TEM D SER NO DRIVER
    %   usermanager ! {ready_to_go, Pid},
    %   Driver = lists:keyfind(isdriver, 1, TripData),
    %   % Driver ! {user_wants_to_go, DriverPid},

    %   io:format("driver_arrived~n"),
    %   handleTripManager(DriversList, PassengersList);

    {new_driver, Pid, X, Y} -> %% Add the driver to the list
      Pid ! {waiting_for_clients},
      io:format("waiting for clients~n"),
      NewDriversList = [{Pid, X, Y} | DriversList],
      % usermanager ! {}
      handleTripManager(NewDriversList, PassengersList);

    {need_a_trip, Pid, FromX, FromY, ToX, ToY} ->
      % Add the passenger to the list
      NewPassengersList = [{Pid, FromX, FromY, ToX, ToY} | PassengersList],

      % Get a driver
      [H | _] = DriversList,
      {PassengerPid, X, Y} = H,
      % Calculate time of Arrival
      DriverDelay = aux:time(aux:distance(X,Y, FromX, FromY)),

      % Signal the user that driver has arrived
      timer:send_after(DriverDelay*1000, Pid, {driver_arrived}),

      handleTripManager(DriversList, NewPassengersList)
  end.

% makeTrip(DriverPid, PassengerPid) ->


% cancelTrip(DriverPid, PassengerPid) ->
