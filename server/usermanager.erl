-module(usermanager).
-export([userManager/2, handleTripManager/2]).

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

handleTripManager(DriversList, PassengersList) ->
  receive
    % {driver_arrived, Pid} -> ISTO AQUI TEM D SER NO DRIVER
    %   usermanager ! {ready_to_go, Pid},
    %   Driver = lists:keyfind(isdriver, 1, TripData),
    %   % Driver ! {user_wants_to_go, DriverPid},

    %   io:format("driver_arrived~n"),
    %   handleTripManager(DriversList, PassengersList);

    {driver_available, Pid, X, Y} -> %% Add the driver to the list
      Pid ! {waiting_for_clients},
      io:format("waiting for clients~n"),
      NewDriversList = [{Pid, X, Y} | DriversList],
      handleTripManager(NewDriversList, PassengersList);

    {need_a_trip, Pid, FromX, FromY, ToX, ToY} ->
      % Add the passenger to the list
      NewPassengersList = [{Pid, FromX, FromY, ToX, ToY} | PassengersList],

      % Get a driver
      [H | _] = DriversList,
      {PassengerPid, X, Y} = H,
      % Calculate time of Arrival
      DriverDelay = aux:time(aux:distance(X,Y, FromX, FromY)),

      % Signal the user that driver has arrived
      timer:send_after(DriverDelay*1000, Pid, {driver_arrived}),

      handleTripManager(DriversList, NewPassengersList)
  end.