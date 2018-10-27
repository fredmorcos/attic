-module(tut6).
-export([list_max/1]).

list_max([Head | Rest]) ->
    list_max(Rest, Head).

list_max([], Res) ->
    Res;
list_max([Head | Rest], Acc) when Head > Acc ->
    list_max(Rest, Head);
list_max([_ | Rest], Acc) ->
    list_max(Rest, Acc).
