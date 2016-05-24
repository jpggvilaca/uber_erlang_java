-module(tripmanager).
-export([tripManager/3]).

%% Handles trip requests
%% Adds passengers and drivers to the lists
tripManager(DriversList, PassengersList, TripList) ->
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

          % Add a tuple to the TripList
          NewTripList = [{DriverPid, Pid} | TripList],

          % Signal usermanager to spawn passenger process
          usermanager ! {passenger_added, Pid},

          % Loop
          tripManager(DriversList, [Passenger | PassengersList], NewTripList);

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
          tripManager([NewDriver | DriversList], PassengersList, TripList)
      end;
    {tcp_response, Pid, Data} ->
      DataAux = string:tokens(Data,":"),
      case (lists:nth(1, DataAux)) of
        "cancel_trip" ->
          % Get the tuple of this trip
          Driver_Passenger = lists:keyfind(Pid, 2, TripList),

          % Parse it
          {DPid, PPid} = Driver_Passenger,

          % Send the message to the driver
          DPid ! {cancel_request, Pid},

          % Loop
          tripManager(DriversList, PassengersList, TripList);
        "enter_car" ->
          % Get the tuple of this trip
          aux:debug(TripList),
          Driver_Passenger = lists:keyfind(Pid, 2, TripList),

          % Parse it
          {DPid, PPid} = Driver_Passenger,

          % Get the Driver and Passenger
          Driver = lists:keyfind(DPid, 1, DriversList),
          Passenger = lists:keyfind(Pid, 1, PassengersList),
          {_, DX, DY,_,_} = Driver,
          {_, FromX, FromY, _, _} = Passenger,

          Distance = aux:distance(DX, DY, FromX, FromY),
          Delay = aux:time(Distance),

          % Start trip
          timer:send_after(Delay*1000, PPid, {trip_ended}),
          timer:send_after(Delay*1000, DPid, {trip_ended}),

          % Loop
          tripManager(DriversList, PassengersList, TripList)
      end
  end.
