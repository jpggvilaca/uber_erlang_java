-module(loginmanager).
-export([loginManager/0]).

%% Handle login and register
loginManager() ->
  receive
    {request, Pid, Data, UsersList} ->
      DataAux = string:tokens(Data,":"),
      case lists:nth(2, DataAux) of
        "reg" -> %% REGISTO
          case lists:keymember(lists:nth(3, DataAux), 1, UsersList) of
            true ->
              usermanager ! {register_failed, Pid, UsersList},
              io:format("Este user já se encontra registado.~n"),
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
                      io:format("Já tem sessão iniciada ~p~n", [User]),
                      usermanager ! {login_failed, Pid, UsersList},
                      loginManager();
                    (Pw == Pass) ->
                      io:format("Login efectuado com sucesso, bemvindo ~p~n", [User]),
                      NewUsersList = aux:changeLogState(Request, true, UsersList),
                      usermanager ! {login_ok, Pid, NewUsersList},
                      loginManager();
                    true ->
                      io:format("Login falhou"),
                      usermanager ! {login_failed, Pid, UsersList},
                      loginManager()
                  end;
                false ->
                  io:format("Utilizador não registado. Tente novamente.~n"),
                  usermanager ! {login_failed, Pid, UsersList},
                  loginManager()
              end,
              loginManager();

            true ->
              io:format("Não existem utilizadores registados.~n"),
              loginManager()
          end
      end
  end.
