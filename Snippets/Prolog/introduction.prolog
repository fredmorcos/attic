%% facts
sunny.
rainy.
this_is_cool.

%% rules
likes(alice, bob).
likes(mary, jane).
likes(mary, alice).
likes(bob, mary).

%% conclusions
compatible(X, Y) :- likes(X, Y), likes(Y, X).
triangle(X, Y, Z) :- likes(X, Y), likes(Y, Z).
triangle(X, Y, Z) :- likes(Y, X), likes(X, Z).
triangle(X, Y, Z) :- likes(Y, Z), likes(Z, X).
triangle(X, Y, Z) :- likes(Z, Y), likes(Y, X).

%% parents
mother(alice, lea).
mother(john, julia).
mother(lea, alberta).
father(james, alfred).
father(lea, john).

parent(X, Y) :- father(X, Y);
                mother(X, Y).

grandfather(X, Y) :- father(Z, Y), parent(X, Z).
grandmother(X, Y) :- mother(Z, Y), parent(X, Z).

grandparent(X, Y) :- grandfather(X, Y);
                     grandmother(X, Y).

ancestor(X, Y) :- parent(X, Y);
                  (parent(Z, Y), ancestor(X, Z)).

%% graph coloring
%% ?- germany(SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY).
color(red).
color(green).
color(blue).
color(yellow).

neighbor(A, B) :- color(A), color(B), A \= B.

germany(SH, MV, HH, HB, NI, ST, BE, BB, SN, NW, HE, TH, RP, SL, BW, BY) :-
    neighbor(SH, NI), neighbor(SH, HH), neighbor(SH, MV),
    neighbor(HH, NI),
    neighbor(MV, NI), neighbor(MV, BB),
    neighbor(NI, HB), neighbor(NI, BB), neighbor(NI, ST), neighbor(NI, TH),
    neighbor(NI, HE), neighbor(NI, NW),
    neighbor(ST, BB), neighbor(ST, SN), neighbor(ST, TH),
    neighbor(BB, BE), neighbor(BB, SN),
    neighbor(NW, HE), neighbor(NW, RP),
    neighbor(SN, TH), neighbor(SN, BY),
    neighbor(RP, SL), neighbor(RP, HE), neighbor(RP, BW),
    neighbor(HE, BW), neighbor(HE, TH), neighbor(HE, BY),
    neighbor(TH, BY),
    neighbor(BW, BY).

%% taller
taller(bob, mike).
taller(mike, jim).
taller(jim, george).

is_taller(X, Y) :- taller(X, Y);
                   taller(X, Z), is_taller(Z, Y).

%% road
get(town1, town2).
get(town2, town3).
get(town3, town4).
get(town4, town5).
get(town5, town6).

can_get(X, Z) :- get(X, Z).
can_get(X, Z) :- get(X, Y), can_get(Y, Z).

%% lists
p([H|T], H, T).

elementof(Item, [Item|_]).
elementof(Item, [_|Tail]) :- elementof(Item, Tail).

append([], L, L).
append(L, [], L).
append([H|T], L, [H|Result]) :- append(T, L, Result).

%% filter([], []).
%% filter([H|T], [H|Res]) :- H > 6, filter(T, Res).
%% filter([_|T], Res) :- filter(T, Res).

lt6(X) :- X > 6.

filter(_, [], []).
filter(P, [H|T], [H|Res]) :- call(P, H), filter(P, T, Res).
filter(P, [_|T], Res) :- filter(P, T, Res).

delete([], _, []).
delete([X|T], X, Res) :- delete(T, X, Res).
delete([H|T], X, [H|Res]) :- delete(T, X, Res).

replace([], _, _, []).
replace([X|T], X, Y, [Y|Res]) :- replace(T, X, Y, Res).
replace([H|T], X, Y, [H|Res]) :- H \= X, replace(T, X, Y, Res).
