-module(usermanager).
-export([userManager/1]).

%% Handles tcp response from user pre-register/login
%% Acts between login/trip managers and the user
userManager(UsersList) ->
  receive
    {enter, Pid} ->
      io:format("User entered with pid: ~p~n", [Pid]),
      userManager(UsersList);

    {tcp_response, Pid, Data} ->
      DataAux = string:tokens(Data, ":"),
      Step = hd(DataAux),

      case Step of
        "1" -> % Delegates control to loginManager
          loginmanager ! {request, Pid, Data, UsersList};
        "2" -> % Delegates control to tripManager
          tripmanager ! {request, Pid, Data, UsersList}
      end,
      userManager(UsersList);

    {register_ok, Pid, NewUsersList} ->
      Pid ! {register_ok},
      userManager(NewUsersList);
    {register_failed, Pid, UsersList} ->
      Pid ! {register_failed},
      userManager(UsersList);
    {login_ok, Pid, NewUsersList} ->
      Pid ! {login_ok},
      userManager(NewUsersList);
    {login_failed, Pid, UsersList} ->
      Pid ! {login_failed},
      userManager(UsersList);

    % {driver_added, Pid, NewDriver} ->
    %   Pid ! {driver_added, NewDriver, PassengersList},
    %   userManager(UsersList);
    % {driver_info, Pid, PassengerPid} ->
    %   Pid ! {driver_info},
    %   userManager(UsersList);
    % {passenger_added, Pid, Passenger} ->
    %   Pid ! {passenger_added, Passenger, DriversList},
    %   userManager(UsersList);

    {leave, Pid} ->
      User = aux:search_user_by_pid(Pid, UsersList),
      io:format("User ~p left~n", [User]),
      io:format("List of active users: ~p~n", [UsersList -- [User]]),
      userManager(UsersList)
  end.