-module(tripmanager).
-export([tripManager/0]).

%% Handles trip requests
tripManager() ->
  receive
    {request, Pid, Data, UsersList} ->
      DataAux = string:tokens(Data,":"),

      case (lists:nth(2, DataAux)) of
        %% Passenger
        "want_trip" ->
          % Get this passenger data
          PassengerData = aux:formatPassengerTrip(DataAux),
          {FromX, FromY, ToX, ToY} = PassengerData,
          Passenger = {Pid, FromX, FromY, ToX, ToY},

          % Spawn new passenger
          % PassengerPid = spawn(fun() -> passenger(Passenger) end),
          Driver = aux:search_user_by_pid(Pid, UsersList),

          % Signal the client
          usermanager ! {passenger_added, Pid, Passenger},
          ChosenDriver = aux:check_for_drivers(UsersList),
          {DriverPid, _, _, _, M, L, _} = ChosenDriver,
          % {DriverPid,_,_,_,_} = H,
          % DriverPid ! {trip_request, PassengerPid, FromX, FromY},

          % loop
          tripManager();
          % This user becomes a passenger
          % passenger(Passenger, Driver);

        %% Driver
        "can_drive" ->
          % Get driver location (x,y), model and licence
          DriverData = aux:formatDriverTrip(DataAux),

          % Add this driver to the list
          DriverInfo = aux:search_user_by_pid(Pid, UsersList),
          {_, _, _, _, M, L, _} = DriverInfo,
          {X, Y} = DriverData,
          NewDriver = {Pid, X, Y, M, L},

          % Spawn new driver
          % DriverPid = spawn(fun() -> driver(NewDriver) end),

          % Signal the client
          usermanager ! {driver_added, Pid, NewDriver},

          % Loop
          tripManager()
          % This user becomes a driver
          % driver(NewDriver)
      end;

    {error} ->
      io:format("TripManager error~n")

  end.
