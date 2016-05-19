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
          io:format("DriversList: ~p", [DriversList]),
          % Get this passenger data
          PassengerData = aux:formatPassengerTrip(DataAux),
          {FromX, FromY, ToX, ToY} = PassengerData,
          Passenger = {Pid, FromX, FromY, ToX, ToY},
          NewPassengersList = [Passenger | PassengersList],

          % Spawn new passenger
          PassengerPid = spawn(fun() -> passenger(Passenger) end),
          register(activepassenger, PassengerPid),

          % Signal the client
          usermanager ! {passenger_added, Pid, PassengerPid},
          [H|T] = DriversList,
          {DriverPid,_,_} = H,
          activedriver ! {trip_request, Pid, FromX, FromY},

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
          DriverPid = spawn(fun() -> driver(NewDriver) end),
          register(activedriver, DriverPid),

          % Signal the client
          usermanager ! {driver_added, Pid, DriverPid},

          % Loop
          tripManager(NewDriversList, PassengersList)
      end;

    {error} ->
      io:format("TripManager error~n")

  end.

%% Handles driver logic
driver(Driver) ->
  receive
    % Data should have the passenger pid and X1,Y1,X2,Y2
    {trip_request, Pid, FromX, FromY} ->
      % Parse Data
      {DPid, DX, DY} = Driver,

      % Calculate delay
      Distance = aux:distance(DX, DY, FromX, FromY),
      Delay = aux:time(Distance),

      io:format("Delay: ~p~n", [Delay]),

      % Send message to passenger warning the arrival
      timer:send_after(Delay*1000, Pid, {driver_arrived, Driver}),

      % Loop
      driver(Driver);

    {cancel_request, Data} ->
      driver(Driver)
  end.

%% Handles passenger logic
passenger(Pid) ->
  receive
    % Data should have the driver pid and X, Y
    {driver_arrived, Driver} ->
      io:format("driver chegou!!~n")
  end.


% trip(Passenger, Driver) ->
%   {start} ->
