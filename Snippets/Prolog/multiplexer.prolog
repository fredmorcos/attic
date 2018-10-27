%% mux([X,_,_,_], [0,0], X).
%% mux([_,X,_,_], [0,1], X).
%% mux([_,_,X,_], [1,0], X).
%% mux([_,_,_,X], [1,1], X).

mux(Values, Select, Output) :- is_list(Values),
                               member(Output, Values),
                               nth0(Select, Values, Output).

switch1(Values, S, Out) :- is_list(Values),
                           nth0(S, Values, Out).
switch2(Values, Sel0, Sel1, Out0, Out1) :- is_list(Values),
                                           Sel0 \= Sel1,
                                           nth0(Sel0, Values, Out0),
                                           nth0(Sel1, Values, Out1),
                                           Out0 \= Out1.
