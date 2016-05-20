-module(usermanager).
-export([userManager/2]).

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
          tripmanager ! {request, Pid, Data}
      end,
      userManager(UsersList, DriversList);

    {register_ok, Pid, NewUsersList} ->
      Pid ! {register_ok},
      userManager(NewUsersList, DriversList);
    {register_failed, Pid, UsersList} ->
      Pid ! {register_failed},
      userManager(UsersList, DriversList);
    {login_ok, Pid, UsersList} ->
      Pid ! {login_ok},
      userManager(UsersList, DriversList);
    {login_failed, Pid, UsersList} ->
      Pid ! {login_failed},
      userManager(UsersList, DriversList);

    {driver_added, Pid, DriverPid} ->
      Pid ! {driver_added},
      userManager(UsersList, DriversList);
    {passenger_added, Pid, PassengerPid} ->
      Pid ! {passenger_added},
      userManager(UsersList, DriversList);

    {leave, _} ->
      io:format("User left~n", []),
      userManager(UsersList, DriversList)
  end.