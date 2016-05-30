-module(tripmanager).
-export([tripManager/4]).

%% Handles trip requests
%% Adds passengers and drivers to the lists
tripManager(DriversList, PassengersList, TripList, TimerList) ->
  receive
    {request, Pid, Data, UsersList} ->
      DataAux = string:tokens(Data,":"),

      case (lists:nth(2, DataAux)) of
        %% Add passenger to list, remove driver from list, add tuple to triplist
        "want_trip" ->
          % Get this passenger data
          PassengerData = aux:formatPassengerTrip(DataAux),
          {FromX, FromY, ToX, ToY} = PassengerData,
          Passenger = {Pid, FromX, FromY, ToX, ToY},

          % Signal usermanager to spawn passenger process
          usermanager ! {passenger_added, Pid},

          if
            length(DriversList) > 0 ->
              % Search for the closest driver
              Driver = aux:get_closest_driver(DriversList, FromX, FromY),

              % Get driver's pid
              {DriverPid, _, _, _, _} = Driver,

              % Send a trip request to the driver with the passenger info
              DriverPid ! {trip_request, Driver, Passenger, FromX, FromY},

              % Driver deleted frm the driverslist array and added to the triplist
              tripManager(DriversList -- [Driver], [Passenger | PassengersList], [{DriverPid, Pid} | TripList], TimerList);
            true ->
              usermanager ! {no_drivers_available, Pid},
              NewTripList = [{is_waiting, Pid} | TripList],
              tripManager(DriversList, [Passenger | PassengersList], NewTripList, TimerList)
          end;
        %% Add Driver to DriverList,
        "can_drive" ->
          % Get driver location (x,y), model and licence
          DriverData = aux:formatDriverTrip(DataAux),

          % Add this driver to the list
          DriverInfo = aux:search_user_by_pid(Pid, UsersList),
          {_, _, _, _, M, L, _} = DriverInfo,
          {X, Y} = DriverData,
          NewDriver = {Pid, X, Y, M, L},

          % Signal usermanager that a driver was added and signal waiting passengers if any
          usermanager ! {driver_added, Pid},
          Request = lists:keyfind(is_waiting, 1, TripList),
          case Request of
            {_, PPid} ->
              usermanager ! {driver_available, PPid},
              NewPassenger = aux:search_user_by_pid(PPid, PassengersList),
              {_, FX, FY, _, _} = NewPassenger,
              Pid ! {trip_request, NewDriver, NewPassenger, FX, FY},
              NewTripList = lists:keyreplace(is_waiting, 1, TripList, {Pid, PPid}),
              tripManager(DriversList, PassengersList, NewTripList, TimerList);
            false ->
              true
          end,

          % Loop
          tripManager([NewDriver | DriversList], PassengersList, TripList, TimerList)
      end;
    {tcp_response, Pid, Data} ->
      DataAux = string:tokens(Data,":"),
      case (lists:nth(1, DataAux)) of
        %% Remove tuple from TripList, Add that driver to driverslist
        "cancel_trip" ->
          % Get the tuple of this trip
          Driver_Passenger = lists:keyfind(Pid, 2, TripList),

          % Parse it
          {DPid, _} = Driver_Passenger,

          % Get the Driver
          ActualDriver = aux:search_user_by_pid(DPid, DriversList),

          % Check trip state
          TripState = lists:keyfind(DPid, 2, TimerList),
          case TripState of
            {ActualTimer, _, PastMoment, Delay} ->
              % Check if 1 minute has passed
              Now = now(),
              TimeElapsed = timer:now_diff(Now, PastMoment) / 1000000,
              if
                TimeElapsed > 60 ->
                  HasToPay = true,
                  io:format("You have to pay half the price~n");
                true ->
                  HasToPay = false
              end,

              % Check if drivertimer is still going
              if
                TimeElapsed < Delay ->
                  {ok, TRef} = ActualTimer,
                    timer:cancel(TRef),
                    DPid ! {cancel_trip_before_time, Pid},
                    Pid ! {cancel_trip_before_time, HasToPay},
                    tripmanager ! {trip_canceled_before_time, DPid},
                    tripManager(DriversList, PassengersList, TripList -- [Driver_Passenger], TimerList -- [TripState]);
                  true ->
                    io:format("Não Cancelei o timer~n"),
                    DPid ! {cancel_trip, Pid},
                    Pid ! {cancel_trip, HasToPay},
                    tripManager(DriversList -- [ActualDriver], PassengersList, TripList -- [Driver_Passenger], TimerList -- [TripState])
              end,
              tripManager(DriversList -- [ActualDriver], PassengersList, TripList -- [Driver_Passenger], TimerList);
            false ->
              io:format("TripState not found!"),
              tripManager(DriversList, PassengersList, TripList, TimerList)
          end;

        %% At the end of the trip remove tuple from triplist and add driver to driverslist
        "start_trip" ->
          % Get the tuple of this trip
          Driver_Passenger = lists:keyfind(Pid, 2, TripList),

          % Parse it
          {DPid, PPid} = Driver_Passenger,

          % Get the Driver and Passenger
          Passenger = lists:keyfind(Pid, 1, PassengersList),
          {_, FromX, FromY, ToX, ToY} = Passenger,

          Distance = aux:distance(FromX, FromY, ToX, ToY),
          Delay = aux:time(Distance),

          % Start trip
          PPid ! {trip_started},
          DPid ! {trip_started},
          timer:send_after(Delay*1000, PPid, {trip_ended}),
          timer:send_after(Delay*1000, DPid, {trip_ended}),
          timer:send_after(Delay*1000, tripmanager, {trip_ended, DPid}),

          % Loop
          tripManager(DriversList, PassengersList, TripList, TimerList)
      end;

    {trip_state, NewTimer, MomentOfRequest, DriverPid, Delay} ->
      tripManager(DriversList, PassengersList, TripList, [{NewTimer, DriverPid, MomentOfRequest, Delay} | TimerList]);

    {trip_canceled_before_time} ->
      io:format("TripManager recebeu trip_canceled_before_time~n"),
      io:format("VIAGEM ACABOU ANTES DO TEMPO, DADOS:~n"),
      io:format("DriversList: ~p~n", [DriversList]),
      io:format("PassengersList: ~p~n", [PassengersList]),
      io:format("TripList: ~p~n", [TripList]),
      io:format("TimerList: ~p~n", [TimerList]),
      tripManager(DriversList, PassengersList, TripList, TimerList);

    {trip_ended, DPid} ->
      io:format("VIAGEM ACABOU NORMALMENTE, DADOS:~n"),
      io:format("DriversList: ~p~n", [DriversList]),
      io:format("PassengersList: ~p~n", [PassengersList]),
      io:format("TripList: ~p~n", [TripList]),
      io:format("TimerList: ~p~n", [TimerList]),
      Driver_Passenger = lists:keyfind(DPid, 1, TripList),
      tripManager(DriversList, PassengersList, TripList -- [Driver_Passenger], TimerList)
  end.
