-module(tut8).
-export([reverse/1]).

reverse(L) ->
    reverse(L, []).

reverse([Head | Tail], Acc) ->
    reverse(Tail, [Head | Acc]);
reverse([], Acc) ->
    Acc.
