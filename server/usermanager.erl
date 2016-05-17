-module(usermanager).
-export([userManager/1]).

%% Intermediary between user and login manager
userManager(UsersList) ->
  receive
    {enter, _} ->
      io:format("User entered~n", []),
      userManager(UsersList);
    {tcp_response, Pid, Data} ->
      DataAux = string:tokens(Data, ":"),
      Step = hd(DataAux),
      case Step of
        "1" -> % login/register requests
          loginmanager ! {request, Pid, Data, UsersList};
        "2" -> % trip requests
          tripmanager ! {request, Pid, Data, UsersList}
      end,
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
      io:format("User left~n", []),
      userManager(UsersList)
  end.