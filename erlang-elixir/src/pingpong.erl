%%%-------------------------------------------------------------------
%%% @author jkaczmarski
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Apr 2025 13:24
%%%-------------------------------------------------------------------
-module(pingpong).
-author("jkaczmarski").

%% API
-export([ping_loop/0, pong_loop/0, start/0, play/1]).

start() ->
  register(ping, spawn(?MODULE, ping_loop, [])),
  register(pong, spawn(?MODULE, pong_loop, [])).

ping_loop() ->
  Timeout = 20000,
  receive
    0 ->
      io:format("ping got 0~n"),
      ping_loop();
    N ->
      io:format("ping got ~w~n", [N]),
      pong ! N - 1,
      ping_loop()
  after
    Timeout -> timeout
  end.

pong_loop() ->
  Timeout = 20000,
  receive
    0 ->
      io:format("pong got 0~n"),
      pong_loop();
    N ->
      timer:sleep(200),
      io:format("pong got ~w~n", [N]),
      ping ! N - 1,
      pong_loop()
  after
    Timeout -> timeout
  end.

play(N) ->
  ping !  N.
