-module(aux).
-define(SPEED, 1). %% 1 unit per minute
-define(PRICE_PER_BLOCK, 5).
-compile(export_all).

distance(FromX, FromY, ToX, ToY) ->
  X1 = list_to_integer(FromX),
  X2 = list_to_integer(ToX),
  Y1 = list_to_integer(FromY),
  Y2 = list_to_integer(ToY),
  abs(X1 - X2) + abs(Y1 - Y2).

is_driver(Username) ->
  string:substr(Username, 1, 2) == "1_".

time(Distance) ->
  round(Distance / ?SPEED).

price(Distance) ->
  Distance * ?PRICE_PER_BLOCK.

formatPassengerData(Data) ->
  Result = {
    lists:nth(3, Data),
    lists:nth(4, Data),
    lists:nth(5, Data)
  },
  Result.

formatDriverTrip(Data) ->
  Result = {
    lists:nth(3, Data),
    lists:nth(4, Data)
  },
  Result.

formatPassengerTrip(Data) ->
  Result = {
    lists:nth(3, Data),
    lists:nth(4, Data),
    lists:nth(5, Data),
    lists:nth(6, Data)
  },
  Result.

formatDriverData(Data) ->
  Result = {
    lists:nth(3, Data),
    lists:nth(4, Data),
    lists:nth(5, Data),
    lists:nth(6, Data),
    lists:nth(7, Data)
  },
  Result.

check_for_drivers(UsersList) ->
  Drivers = lists:keyfind("1", 4, UsersList),
  Drivers.

get_closest_driver(DriversList, PassengerX, PassengerY) ->
  GetDistances = fun({Pid, X, Y, M, L}) ->
    distance(X, Y, PassengerX, PassengerY) end
  ,
  GetDistancesWithPid = fun({Pid, X, Y, M, L}) ->
    {Pid, distance(X, Y, PassengerX, PassengerY)} end
  ,
  Distances = lists:map(GetDistances, DriversList),
  MinimumDistance = lists:min(Distances),
  Drivers = lists:map(GetDistancesWithPid, DriversList),
  ClosestDriverPid = lists:keyfind(MinimumDistance, 2, Drivers),
  {DriverPid, Dist} = ClosestDriverPid,
  ClosestDriver = lists:keyfind(DriverPid, 1, DriversList),
  ClosestDriver.

set_alarm(Time, Msg) ->
  spawn(timer, set, [self(),Time,Msg]).

% set(Pid, Time, Alarm) ->
%   receive
%   after
%     Time ->
%       Pid ! Alarm
%   end.

prepend(X, {}) -> {X};
prepend(X, {A}) -> {X, A}.

append(X, {}) -> {X};
append(X, {A}) -> {A, X}.

debug(Var) ->
  io:format("Var:~p~n", [Var]).

search_user_by_pid(Pid, List) ->
  User = lists:keyfind(Pid, 1, List),
  User.

changeLogState(User, NewState, UsersList) ->
  {Pid, Us, Pw, Ty, Mod, Lic,State} = User,
  NewList = lists:keyreplace(State, 7, UsersList, {Pid, Us, Pw, Ty, Mod, Lic,NewState}),
  NewList.
