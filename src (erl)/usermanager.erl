-module(usermanager).
-export([userManager/1]).

%% Intermediary between user and login manager
userManager(UsersList) ->
  receive
    {enter, _} ->
      io:format("User entered~n", []),
      userManager(UsersList);
    {tcp_response, Pid, Data} ->
      loginmanager ! {request, Pid, Data, UsersList},
      userManager(UsersList);
    {register_ok, Pid, NewUsersList} ->
      Pid ! {register_ok},
      userManager(NewUsersList);
    {login_ok, Pid, NewUsersList} ->
      Pid ! {login_ok},
      userManager(NewUsersList);
    {login_failed, Pid, NewUsersList} ->
      Pid ! {login_failed},
      userManager(NewUsersList);
    {leave, _} ->
      io:format("User left~n", [])
  end.