-module(tripmanager).
-export([tripManager/2]).

%% Handles trip requests
tripManager(DriversList, PassengersList) ->
  receive
    {request, Pid, Data, UsersList} ->
      DataAux = string:tokens(Data,":"),

      case (lists:nth(2, DataAux)) of
        %% Passenger
        "want_trip" ->
          % Get this passenger data and add it to the list
          PassengerData = aux:formatPassengerTrip(DataAux),
          {FromX, FromY, ToX, ToY} = PassengerData,
          Passenger = {Pid, FromX, FromY, ToX, ToY},
          NewPassengersList = [Passenger | PassengersList],

          % Spawn new passenger
          PassengerPid = spawn(fun() -> passenger(Passenger) end),

          % Signal the client
          usermanager ! {passenger_added, Pid, PassengerPid},
          [H|T] = DriversList,
          {DriverPid,_,_,_,_} = H,
          DriverPid ! {trip_request, PassengerPid, FromX, FromY},

          % Testing
          aux:debug(Pid), % o meu
          aux:debug(PassengerPid), % o do processo passenger
          aux:debug(DriverPid), % o do condutor

          % loop
          tripManager(DriversList, NewPassengersList);

        %% Driver
        "can_drive" ->
          % Get driver location (x,y), model and licence
          DriverData = aux:formatDriverTrip(DataAux),

          % Add this driver to the list
          DriverInfo = aux:search_driver_by_pid(Pid, UsersList),
          {_, _, _, _, M, L, _} = DriverInfo,
          {X, Y} = DriverData,
          NewDriver = {Pid, X, Y, M, L},
          NewDriversList = [NewDriver | DriversList],

          % Spawn new driver
          DriverPid = spawn(fun() -> driver(NewDriver) end),

          % Signal the client
          usermanager ! {driver_added, Pid, DriverPid},

          % Testing
          aux:debug(Pid), % o meu
          aux:debug(DriverPid), % o do processo driver

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
      usermanager ! {driver_info, DPid}, % why?

      % Send message to passenger warning the arrival
      timer:send_after(Delay*5000, PassengerPid, {driver_arrived, Driver}),

      % Loop
      driver(Driver);

    {cancel_request, Data} ->
      driver(Driver)
  end.

%% Handles passenger logic
passenger(Passenger) ->
  receive
    % Data should have the driver pid and X, Y
    {driver_arrived, Driver} ->
      io:format("driver chegou!!~n"),
      io:format("Driver: ~p", [Driver]);
    {driver_info, Distance, Delay, Price, Model, Licence} ->
      io:format("informação do driver chegou!!~n")
      % cancel or enter car

  end.


% trip(Passenger, Driver) ->
%   receive
%     {start} ->

