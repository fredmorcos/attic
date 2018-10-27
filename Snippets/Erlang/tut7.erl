-module(tut7).
-export([format_temps/1]).

format_temps(Cities) ->
    ConvertedCities = convert_list_to_c(Cities),
    print_temp(ConvertedCities),
    {MaxCity, MinCity} = find_max_and_min(ConvertedCities),
    print_max_and_min(MaxCity, MinCity).

convert_list_to_c([{Name, {f, F}} | Tail]) ->
    ConvertedCity = {Name, {c, (F - 32) * 5 / 9}},
    [ConvertedCity | convert_list_to_c(Tail)];
convert_list_to_c([City | Tail]) ->
    [City | convert_list_to_c(Tail)];
convert_list_to_c([]) ->
    [].

print_temp([{Name, {c, Temp}} | Tail]) ->
    io:format("~-15w ~w c~n", [Name, Temp]),
    print_temp(Tail);
print_temp([]) ->
    ok.

find_max_and_min([City | Tail]) ->
    find_max_and_min(Tail, City, City).

find_max_and_min([{Name, {c, Temp}} | Tail],
                 {MaxName, {c, MaxTemp}},
                 {MinName, {c, MinTemp}}) ->
    if Temp > MaxTemp ->
            MaxCity = {Name, {c, Temp}};
       true ->
            MaxCity = {MaxName, {c, MaxTemp}}
    end,
    if Temp < MinTemp ->
            MinCity = {Name, {c, Temp}};
       true ->
            MinCity = {MinName, {c, MinTemp}}
    end,
    find_max_and_min(Tail, MaxCity, MinCity);
find_max_and_min([], MaxCity, MinCity) ->
    {MaxCity, MinCity}.

print_max_and_min({MaxName, {c, MaxTemp}}, {MinName, {c, MinTemp}}) ->
    io:format("Max temperature was ~w c in ~w~n", [MaxTemp, MaxName]),
    io:format("Min temperature was ~w c in ~w~n", [MinTemp, MinName]).
