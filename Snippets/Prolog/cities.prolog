%% ?- votesFor([Cairo, London, Beijing, Moscow, Mumbai, Nairobi, Jakarta]).

votesFor([Cairo, London, Beijing, Moscow, Mumbai, Nairobi, Jakarta]) :-
    permutation([Cairo, London, Beijing, Moscow, Mumbai, Nairobi, Jakarta],
                [4,2,2,1,1,0,0]),
    Cairo \= Beijing,
    (Moscow = 4; Moscow = 0),
    Cairo > Jakarta,
    find([Cairo, London, Beijing, Moscow, Mumbai, Nairobi, Jakarta], [0, 2], 2),
    (Jakarta is (London - 1); Jakarta is (Beijing - 1)).

find([], _, 0).
find([X, Y|Rest], [X, Y], N) :- find(Rest, [X, Y], Nx), N is Nx + 1.
find([Z|Rest], [X, Y], N) :- Z \= X, find(Rest, [X, Y], N).
