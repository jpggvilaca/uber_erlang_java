-module(loginmanager).
-export([loginManager/0]).

%% Handle login and register
loginManager() ->
  receive
    {request, Pid, Data, UsersList} ->
      DataAux = string:tokens(Data,":"),
      case lists:nth(2, DataAux) of
        "reg" -> %% REGISTO
          if
            length(DataAux) > 5 ->
              {User,Pw,Type,Model,Licence} = aux:formatDriverData(DataAux),
              NewUsersList = [{User,Pw,Type,Model,Licence}|UsersList],
              usermanager ! {register_ok, Pid, NewUsersList};
            length(DataAux) =< 5 ->
              {User,Pw,Type} = aux:formatPassengerData(DataAux),
              NewUsersList = [{User,Pw,Type,"",""}|UsersList],
              usermanager ! {register_ok, Pid, NewUsersList}
          end,
          loginManager();
        "log" -> %% LOGIN
          {User,Pw, _} = aux:formatPassengerData(DataAux),
          if
            length(UsersList) /= 0 ->
              Request = lists:keyfind(User, 1, UsersList),
              case Request of
                {_, Pass,_,_,_} ->
                  if
                    Pw == Pass ->
                      io:format("Login efectuado com sucesso, bemvindo ~p~n", [User]),
                      usermanager ! {login_ok, Pid, UsersList},
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
