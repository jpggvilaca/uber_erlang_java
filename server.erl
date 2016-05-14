-module(server).
-author('Joao Vilaca').
-export([start/1]).

-define(TCP_OPTIONS, [list, {packet, 0} , {reuseaddr, true}]).

start(Port) ->
  {ok, ListenSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
  acceptor(ListenSocket).

acceptor(ListenSocket) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  UserList = dict:new(),
  User = spawn(fun() -> users(UserList) end),
  Loop = spawn(fun() -> loop(Socket) end),
  register(userServer, User),
  acceptor(ListenSocket).

loop(Socket) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, Data} ->
      MAux = string:tokens(Data," "),
      case hd(MAux) of
        "create" ->
          userServer ! {create, Socket, "gen", "baderous"},
          loop(Socket);
        _ ->
          io:format("Received: ~s~n", [MAux]),
          loop(Socket)
      end;
    {error, closed} ->
      io:format("error...~n"),
      ok
  end.

users(UserList) ->
  receive
    {create, Socket, User, Pass} ->
      UserList:append(User, Pass),
      io:format("criando user...~s,~s~n",[User,Pass]),
      % users(UserList);
      create_user(User, Pass)
    % {From, {list}} ->
    %   From ! { userServer , string:join(UserList, ", ")}
      % users(UserList)
  end.


create_user(Username, Password) ->
  io:format("Create_user...~s,~s~n",[Username,Password]),
  userServer ! { self(), user_created_success}.

% list_users() ->
%   io:format("list users...~n"),
%   userServer ! { self(), {list}},
%   receive
%     { userServer, Response } ->
%       Response
%   end.