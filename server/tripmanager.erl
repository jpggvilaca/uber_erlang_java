-module(tripmanager).
-export([tripManager/2]).

%% Handles trip requests
%% Adds passengers and drivers to the lists
tripManager(DriversList, PassengersList) ->
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

          % Search for the closest driver
          Driver = aux:search_user_by_pid(Pid, UsersList),

          % test
          Cenas = aux:get_closest_driver(DriversList, FromX, FromY),
          aux:debug(Cenas),

          % Send this passenger's info to usermanager
          usermanager ! {passenger_added, Pid, Passenger},

          {DriverPid, _, _, _, M, L, _} = Driver,
          % {DriverPid,_,_,_,_} = H,
          % DriverPid ! {trip_request, PassengerPid, FromX, FromY},

          % loop
          tripManager(DriversList, PassengersList);
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
          tripManager([NewDriver | DriversList], PassengersList)
          % This user becomes a driver
          % driver(NewDriver)
      end
  end.
