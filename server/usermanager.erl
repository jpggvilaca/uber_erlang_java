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

    % Register / Login
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
    {login_failed_user_already_exists, Pid, UsersList} ->
      Pid ! {login_failed_user_already_exists},
      userManager(UsersList);
    {login_failed_user_doesnt_exist, Pid, UsersList} ->
      Pid ! {login_failed_user_doesnt_exist},
      userManager(UsersList);
    {login_failed_wrong_password, Pid, UsersList} ->
      Pid ! {login_failed_wrong_password},
      userManager(UsersList);

    % Type of user
    {user_is_driver, Pid} ->
      Pid ! {user_is_driver},
      userManager(UsersList);
    {user_is_passenger, Pid} ->
      Pid ! {user_is_passenger},
      userManager(UsersList);

    % Pre-Trip
    {driver_added, Pid} ->
      Pid ! {driver_added},
      userManager(UsersList);
    {passenger_added, Pid} ->
      Pid ! {passenger_added},
      userManager(UsersList);
    {driver_available, Pid} ->
      Pid ! {driver_available},
      userManager(UsersList);
    {no_drivers_available, Pid} ->
      Pid ! {no_drivers_available},
      userManager(UsersList);

    % Trip

    {leave, Pid} ->
      User = aux:search_user_by_pid(Pid, UsersList),
      io:format("User ~p left~n", [User]),
      io:format("List of active users: ~p~n", [UsersList -- [User]]),
      userManager(UsersList)
  end.