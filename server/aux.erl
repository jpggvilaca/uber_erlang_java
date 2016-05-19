-module(aux).
-define(SPEED, 1). %% 1 unit per minute
-define(PRICE_PER_BLOCK, 5).
-export([
  prepend/2,
  distance/4,
  is_driver/1,
  time/1,
  price/1,
  formatPassengerData/1,
  formatDriverData/1,
  formatPassengerTrip/1,
  formatDriverTrip/1,
  check_for_drivers/1,
  search_driver_by_pid/2,
  set_alarm/2]).

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
  Drivers = lists:keyfind("1", 3, UsersList),
  Drivers.

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

% add_driver({Pid, X, Y}, DriversList) ->
%   NewDriversList = [{Pid, X, Y} | DriversList],
%   NewDriversList.

% remove_driver({Pid, X, Y}, DriversList) ->
%   [H|T] = DriversList,
%   T.

search_driver_by_pid(Pid, DriversList) ->
  Driver = lists:keyfind(Pid, 1, DriversList),
  Driver.