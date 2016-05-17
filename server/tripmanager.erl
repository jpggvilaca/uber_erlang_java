-module(tripmanager).
-export([tripManager/0]).

%% Handles trip requests
tripManager() ->
  receive
    {request, Pid, Data, Users} ->
      DataAux = string:tokens(Data,":"),
      Driver = aux:check_for_drivers(Users),

      case (lists:nth(2, DataAux)) of
        %% Passenger
        "want_trip" ->
          % Get a driver, his home and the time for him to come
          % {Name, Car, Type, Model, Licence} = Driver,
          % [H|T] = Users,
          % {DriverName, DriverCar, {X, Y}} = H,

          % Get this passenger data
          % PassengerData = aux:formatPassengerTrip(DataAux),
          % {FromX, FromY, ToX, ToY} = PassengerData,

          % Calculate distance and cost
          % isto não pode ser aqui
          % DriverDelay = aux:time(aux:distance(X,Y, FromX, FromY)),
          % Distance = aux:distance(FromX, FromY, ToX, ToY),
          % Time = aux:time(Distance),
          % Price = aux:price(Distance),
          % timer:send_after(Time*1000, handletripmanager, {driver_arrived, Pid}),


          % while driver doesnt come
            % can cancel in the next 1min without cost
            % or after the 1min with a cost

          % confirm entering

          % make the trip


          tripManager();


        %% Driver
        "can_drive" ->
          DriverData = aux:formatDriverTrip(DataAux),

          % Add this driver to the list
          {Name, Car, Type, Model, Licence} = Driver,
          {X, Y} = DriverData,
          % NewDriversList = [{Name, Car, {X, Y}}|Users],

          % Wait for passengers to call
          handletripmanager ! {driver_available, Pid, X, Y},
          tripManager()
      end,

      tripManager();

    {error} ->
      io:format("TripManager error~n")

  end.