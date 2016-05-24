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
          Driver = aux:get_closest_driver(DriversList, FromX, FromY),

          % Get driver's pid
          {DriverPid, _, _, _, _} = Driver,

          % Send a trip request to the driver with the passenger info
          DriverPid ! {trip_request, Driver, Passenger, FromX, FromY},

          % Signal usermanager to spawn passenger process
          usermanager ! {passenger_added, Pid},

          % Loop
          tripManager(DriversList, [Passenger | PassengersList]);

        %% Driver
        "can_drive" ->
          % Get driver location (x,y), model and licence
          DriverData = aux:formatDriverTrip(DataAux),

          % Add this driver to the list
          DriverInfo = aux:search_user_by_pid(Pid, UsersList),
          {_, _, _, _, M, L, _} = DriverInfo,
          {X, Y} = DriverData,
          NewDriver = {Pid, X, Y, M, L},

          % Signal usermanager to spawn driver process
          usermanager ! {driver_added, Pid},

          % Loop
          tripManager([NewDriver | DriversList], PassengersList)
      end
  end.
