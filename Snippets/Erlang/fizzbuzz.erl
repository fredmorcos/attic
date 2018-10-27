-module(fizzbuzz).
-export([fizzbuzz/0]).

fizzbuzz() ->
    fizzbuzz(1, 100).

fizzbuzz(X, Max) when X == Max ->
    ok;
fizzbuzz(X, Max) ->
    if X rem 3 == 0 andalso X rem 5 == 0 ->
            io:format("FizzBuzz~n");
       X rem 3 == 0 ->
            io:format("Fizz~n");
       X rem 5 == 0 ->
            io:format("Buzz~n");
       true ->
            io:format("~w~n", [X])
    end,
    fizzbuzz(X + 1, Max).
