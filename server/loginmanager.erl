-module(loginmanager).
-export([loginManager/0]).

%% Handle login and register
loginManager() ->
  receive
    {request, Pid, Data, UsersList} ->
      DataAux = string:tokens(Data,":"),
      case lists:nth(2, DataAux) of
        "reg" -> %% REGISTO
          case lists:nth(5, DataAux) of
            "1" ->
              usermanager ! {user_is_driver, Pid};
            "2" ->
              usermanager ! {user_is_passenger, Pid}
          end,
          case lists:keymember(lists:nth(3, DataAux), 1, UsersList) of
            true ->
              usermanager ! {register_failed, Pid, UsersList},
              UserDoesntExist = false;
            false ->
              UserDoesntExist = true
          end,
          if
            (length(DataAux) > 5) and UserDoesntExist ->
              {User,Pw,Type,Model,Licence} = aux:formatDriverData(DataAux),
              NewUsersList = [{Pid,User,Pw,Type,Model,Licence,false}|UsersList],
              usermanager ! {register_ok, Pid, NewUsersList};

            (length(DataAux) =< 5) and UserDoesntExist ->
              {User,Pw,Type} = aux:formatPassengerData(DataAux),
              NewUsersList = [{Pid,User,Pw,Type,"","",false}|UsersList],
              usermanager ! {register_ok, Pid, NewUsersList};
            true ->
              loginManager()
          end,
          loginManager();

        "log" -> %% LOGIN
          {User,Pw, _} = aux:formatPassengerData(DataAux),
          if
            (length(UsersList) /= 0) ->
              Request = lists:keyfind(User, 2, UsersList),
              case Request of
                {_,_,Pass,_,_,_,IsLogged} ->
                  if
                    (IsLogged == true) ->
                      usermanager ! {login_failed_user_already_exists, Pid, UsersList},
                      loginManager();
                    (Pw == Pass) ->
                      NewUsersList = aux:changeLogState(Request, true, UsersList),
                      usermanager ! {login_ok, Pid, NewUsersList},
                      loginManager();
                    true ->
                      usermanager ! {login_failed_wrong_password, Pid, UsersList},
                      loginManager()
                  end;
                false ->
                  usermanager ! {login_failed_user_doesnt_exist, Pid, UsersList},
                  loginManager()
              end,
              loginManager();

            true ->
              loginManager()
          end
      end
  end.
