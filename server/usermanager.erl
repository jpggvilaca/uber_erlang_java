-module(usermanager).
-export([userManager/2, handleTripManager/1]).

%% Handles tcp response from user pre-register/login
userManager(UsersList, DriversList) ->
  receive
    {enter, Pid} ->
      io:format("User entered with pid: ~p~n", [Pid]),
      userManager(UsersList, DriversList);

    {tcp_response, Pid, Data} ->
      DataAux = string:tokens(Data, ":"),
      Step = hd(DataAux),

      case Step of
        "1" -> % Delegates control to loginManager
          loginmanager ! {request, Pid, Data, UsersList};
        "2" -> % Delegates control to tripManager
          tripmanager ! {request, Pid, Data, UsersList}
      end,
      userManager(UsersList, DriversList);

    {register_ok, Pid, NewUsersList} ->
      Pid ! {register_ok},
      userManager(NewUsersList, DriversList);
    {login_ok, Pid, NewUsersList} ->
      Pid ! {login_ok},
      userManager(NewUsersList, DriversList);
    {login_failed, Pid, NewUsersList} ->
      Pid ! {login_failed},
      userManager(NewUsersList, DriversList);

    {ready_to_go, Pid} ->
      Pid ! {start_trip},
      userManager(UsersList, DriversList);

    {leave, _} ->
      io:format("User left~n", []),
      userManager(UsersList, DriversList)
  end.

handleTripManager(TripData) ->
  receive
    {driver_arrived, Pid} ->
      usermanager ! {ready_to_go, Pid},
      [{Driver, _, _}|_] = TripData,
      Driver ! {user_wants_to_go},
      io:format("driver_arrived~n"),
      handleTripManager(TripData);

    {driver_available, Pid, X, Y} ->
      Pid ! {waiting_for_clients},
      io:format("waiting for clients~n"),
      NewTripData = [{isdriver, Pid, X, Y}|TripData],
      handleTripManager(NewTripData);

    {need_a_trip, Pid, FromX, FromY, ToX, ToY} ->
      io:format("need_a_trip~n"),
      NewTripData = [{ispassenger, Pid, FromX, FromY, ToX, ToY}|TripData],
      handleTripManager(NewTripData)
  end.