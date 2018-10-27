-module(messagepassing).
-export([start/0, ping/2, pong/0]).

ping(0, PongID) ->
    PongID ! finished,
    io:format("Ping finished~n");
ping(N, PongID) ->
    PongID ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n")
    end,
    ping(N - 1, PongID).

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
    PongID = spawn(messagepassing, pong, []),
    spawn(messagepassing, ping, [3, PongID]).
