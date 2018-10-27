-module(tut13).
-export([convert_list_to_celsius/1]).

convert_to_celsius({Name, {f, Temp}}) ->
    {Name, {c, trunc((Temp - 32) * 5 / 9)}};
convert_to_celsius(X) ->
    X.

convert_list_to_celsius(List) ->
    NewList = lists:map(fun convert_to_celsius/1, List),
    lists:sort(fun({_, {c, Temp1}}, {_, {c, Temp2}}) ->
                       Temp1 < Temp2 end, NewList).
