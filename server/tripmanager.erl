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
          io:format("want_trip!!~n"),
          % get a driver
          {Name, Car, Type, Model, Licence} = Driver,
          % get this passenger data
          PassengerData = aux:formatPassengerTrip(DataAux),
          {FromX, FromY, ToX, ToY} = PassengerData,
          % calculate distance and cost
          Distance = aux:distance(FromX, FromY, ToX, ToY),
          Time = aux:time(Distance),
          Price = aux:price(Distance),
          % while driver doesnt come
            % can cancel in the next 1min without cost
            % or after the 1min with a cost
          % confirm entering
          % make the trip

          tripManager(DriversList);
        "can_drive" ->
          DriverData = aux:formatDriverTrip(DataAux),
          io:format("can_drive!!~n"),
          % add this driver to the list
          {Name, Car, Type, Model, Licence} = Driver,
          {X, Y} = DriverData,
          NewDriversList = [{Name, Car, {X, Y}}|DriversList],
          % wait for passengers to call
          tripManager(NewDriversList)
      end,
      tripManager(DriversList);
    {error} ->
      io:format("drivermanager available_to_drive~n")
  end.

% passengerManager() ->
%   receive
%     {request_trip, Pid, From, To} ->
%       % check for available drivers
%       % calculate trip time
%       % calculate trip cost
%       % signal the chosen driver (closest or first?)
%       % tell user that car arrived to "From" (ask to confirm before taking it)
%       % tell user that they arrived to "To"
%     {cancel_trip, Pid} ->
%       % Signal driver
%       % pay the price if > 1 min has passed
%   end.