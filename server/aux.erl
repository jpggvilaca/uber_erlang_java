-module(aux).
-define(SPEED, 1). %% 1 unit per minute
-define(PRICE_PER_BLOCK, 5).
-export([
  distance/2,
  is_driver/1,
  time/1,
  price/1,
  formatPassengerData/1,
  formatDriverData/1,
  set_alarm/2]).

distance({FromX,FromY}, {ToX, ToY}) ->
  abs(FromX - ToX) + abs(FromY - ToY).

is_driver(Username) ->
  string:substr(Username, 1, 2) == "1_".

time(Distance) ->
  Distance / ?SPEED.

price(Distance) ->
  Distance * ?PRICE_PER_BLOCK.

formatPassengerData(Data) ->
  Result = {
    lists:nth(3, Data),
    lists:nth(4, Data),
    lists:nth(5, Data)
  },
  Result.

formatPassengerTrip(Data) ->
  Result = {
    lists:nth(3, Data),
    lists:nth(4, Data)
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
  Drivers = lists:keyfind("1", 3, UsersList).

set_alarm(Time, Msg) ->
  spawn(timer, set, [self(),Time,Msg]).

set(Pid, Time, Alarm) ->
  receive
  after
    Time ->
      Pid ! Alarm
  end.