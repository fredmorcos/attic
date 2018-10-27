-module(tut5).
-export([format_temps/1]).

format_temps([]) ->
    ok;
format_temps([City | Rest]) ->
    print_temp(convert_to_celsius(City)),
    format_temps(Rest).

convert_to_celsius({Name, {c, Temperature}}) ->
    {Name, {c, Temperature}};
convert_to_celsius({Name, {f, Temperature}}) ->
    {Name, {c, (Temperature - 32) * 5 / 9}}.

print_temp({Name, {c, Temperature}}) ->
    io:format("~-15w ~w c~n", [Name, Temperature]).
