-module(tripmanager).
-export([tripManager/0]).

%% Handles trip requests
tripManager() ->
  receive
    {request, Pid, Data, UsersList} ->
      DataAux = string:tokens(Data,":"),
      case (lists:nth(2, DataAux)) of
        "want_trip" ->
          io:format("want_trip!!~n"),
          tripManager();
        "can_drive" ->
          io:format("can_drive!!~n"),
          tripManager()
      end,
      tripManager();
    {available_to_drive, Pid, Data} ->
      io:format("drivermanager available_to_drive~n")
      % Insert this user on driverslist
      % Wait for trip requests
    % {trip_canceled, Pid} ->
      % Signal driver
      % pay the price if > 1 min has passed
  end.

% passengerManager() ->
%   receive
%     {request_trip, Pid, From, To} ->
%       % check for available drivers
%       % calculate trip time
%       % calculate trip cost
%       % signal the chosen driver (closest or first?)
%       % tell user that car arrived to "From" (ask to confirm before taking it)
%       % tell user that they arrived to "To"
%     {cancel_trip, Pid} ->
%       % Signal driver
%       % pay the price if > 1 min has passed
%   end.