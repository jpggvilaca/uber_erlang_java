-module(tripmanager).
-export([tripManager/1]).

%% Handles trip requests
tripManager(DriversList) ->
  receive
    {request, Pid, Data, Users} ->
      DataAux = string:tokens(Data,":"),
      Driver = aux:check_for_drivers(Users),
      case (lists:nth(2, DataAux)) of
        "want_trip" ->
          % Get a driver, his home and the time for him to come
          {Name, Car, Type, Model, Licence} = Driver,
          [H|T] = DriversList,
          {DriverName, DriverCar, {X, Y}} = H,
          % Get this passenger data
          PassengerData = aux:formatPassengerTrip(DataAux),
          {FromX, FromY, ToX, ToY} = PassengerData,
          % Calculate distance and cost
          DriverDelay = aux:time(aux:distance(X,Y, FromX, FromY)),
          Distance = aux:distance(FromX, FromY, ToX, ToY),
          Time = aux:time(Distance),
          Price = aux:price(Distance),
          io:format("DriverDelay: ~p~n", [DriverDelay]),
          io:format("Distance: ~p~n", [Distance]),
          io:format("Time: ~p~n", [Time]),
          io:format("Price: ~p~n", [Price]),
          timer:send_after(Time*1000, Pid, {driver_ready}),
          % while driver doesnt come
            % can cancel in the next 1min without cost
            % or after the 1min with a cost
          % confirm entering
          % make the trip

          tripManager(DriversList);
        "can_drive" ->
          DriverData = aux:formatDriverTrip(DataAux),
          % Add this driver to the list
          {Name, Car, Type, Model, Licence} = Driver,
          {X, Y} = DriverData,
          NewDriversList = [{Name, Car, {X, Y}}|DriversList],
          % Wait for passengers to call
          tripManager(NewDriversList)
      end,
      tripManager(DriversList);
    {error} ->
      io:format("drivermanager available_to_drive~n")
  end.
