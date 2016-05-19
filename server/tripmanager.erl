-module(tripmanager).
-export([tripManager/2]).

%% Handles trip requests
tripManager(DriversList, PassengersList) ->
  receive
    {request, Pid, Data} ->
      DataAux = string:tokens(Data,":"),
      % Driver = aux:check_for_drivers(Users),

      case (lists:nth(2, DataAux)) of
        %% Passenger
        "want_trip" ->
          % Get this passenger data
          PassengerData = aux:formatPassengerTrip(DataAux),
          {FromX, FromY, ToX, ToY} = PassengerData,
          Passenger = {Pid, FromX, FromY, ToX, ToY},
          NewPassengersList = [Passenger | PassengersList],

          % Spawn new passenger
          spawn(fun() -> passenger(Pid) end),

          % handletripmanager ! {need_a_trip, Pid, FromX, FromY, ToX, ToY},

          % Calculate distance and cost
          % DriverDelay = aux:time(aux:distance(X,Y, FromX, FromY)),
          % Distance = aux:distance(FromX, FromY, ToX, ToY),
          % Time = aux:time(Distance),
          % Price = aux:price(Distance),
          % timer:send_after(Time*1000, handletripmanager, {driver_arrived, Pid}),

          % loop
          tripManager(DriversList, NewPassengersList);

        %% Driver
        "can_drive" ->
          % Get driver location (x,y)
          DriverData = aux:formatDriverTrip(DataAux),

          % Add this driver to the list
          {X, Y} = DriverData,
          NewDriver = {Pid, X, Y},
          NewDriversList = [NewDriver | DriversList],

          % Spawn new driver
          DriverPid = spawn(fun() -> driver(Pid) end),

          % Signal the client
          usermanager ! {driver_added, Pid, DriverPid},

          % Loop
          tripManager(NewDriversList, PassengersList)
      end;

    {error} ->
      io:format("TripManager error~n")

  end.


driver(Pid) ->
  io:format("Driver").
  % receive
  %   % Data should have the passenger pid and X1,Y1,X2,Y2
  %   {trip_request, Data} ->

  %   {cancel_trip, Data} ->

passenger(Pid) ->
  io:format("passenger").
  % receive
    % Data should have the driver pid and X, Y
    % {driver_arrived} ->


% trip(Passenger, Driver) ->
%   {start} ->
