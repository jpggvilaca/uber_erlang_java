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
          % get a driver
          % calculate distance and cost
          % while driver doesnt come
            % can cancel in the next 1min without cost
            % or after the 1min with a cost
          % confirm entering
          % make the trip

          tripManager();
        "can_drive" ->
          io:format("can_drive!!~n"),
          % add this driver to the list
          % wait for passengers to call
          tripManager()
      end,
      tripManager();
    {error} ->
      io:format("drivermanager available_to_drive~n")
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