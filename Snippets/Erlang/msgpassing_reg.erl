-module(messagepassing_reg).
-export([start/0, ping/1, pong/0]).

ping(0) ->
    pong ! finished,
    io:format("Ping finished~n");
ping(N) ->
    pong ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n")
    end,
    ping(N - 1).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n");
        {ping, PingID} ->
            io:format("Pong received ping from ~w~n", [PingID]),
            PingID ! pong,
            pong()
    end.

start() ->
    register(pong, spawn(messagepassing_reg, pong, [])),
    spawn(messagepassing_reg, ping, [3]).
