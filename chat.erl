-module(chat).
-export([server/1]).
-author("João Vilaça").
-define(TCP_OPTIONS, [list, {packet, 0}, {reuseaddr, true}]).

server(Port) ->
  Room = spawn(fun() -> room([]) end), % é criado e processo Room
  {ok, LSock} = gen_tcp:listen(Port, ?TCP_OPTIONS),
  acceptor(LSock, Room). % executa o acceptor

acceptor(LSock, Room) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  spawn(fun() -> acceptor(LSock, Room) end), % é criado mais 1 acceptor
  Room ! {enter, self()}, % o pid deste processo é enviado para room
  user(Sock, Room). % este processo agora é um user, executa o metodo user

room(Pids) ->
  receive
    {enter, Pid} -> % recebendo a msg 'enter' e o pid
      io:format("user entered~n", []), % imprime no ecra
      room([Pid | Pids]); % adiciona o pid à cabeça e entra em loop pronto a receber mais msgs
    {line, _} = Msg -> % atribui a Msg o tuplo line e Data recebida
      [Pid ! Msg || Pid <- Pids], % envia a data para para todo o Pid em Pids
      room(Pids); % loop
    {leave, Pid} -> % ao receber este tuplo
      io:format("user left~n", []), % imprime cenas
      room(Pids -- [Pid]); % retira o pid do array de Pids
    {tcp, _, _} = Msg ->
      [Pid ! Msg || Pid <- Pids], % envia a data para para todo o Pid em Pids
      room(Pids) % loop
  end.

user(Sock, Room) ->
  receive
    {line, Data} -> % ao receber isto
      io:format("Room: ~s~n", [Data]),
      gen_tcp:send(Sock, Data), %% envia para o socket a Data que recebeu
      user(Sock, Room); % loop
    {tcp, _, Data} -> % ao receber cenas do tcp
      Room ! {line, Data}, % manda a data para o room (broadcast)
      user(Sock, Room); %% entra em loop
    {tcp_closed, _} ->
      Room ! {leave, self()}; % manda o pid deste user e 'leave' para room
    {tcp_error, _, _} ->
      Room ! {leave, self()} % manda o pid deste user e 'leave' para room
  end.


