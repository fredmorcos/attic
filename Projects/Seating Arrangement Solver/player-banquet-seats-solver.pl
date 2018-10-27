% Nada Victor, Fatma Ziwar, Fred Morcos

% to load the program players just write the path
% of the program in this format: [fileName].
% for example:      ?- ['C:/Documents and Settings/Ouzie/Desktop/prolog/players'].

player_list([],0).
player_list(L,N):- L=[H|T],
                   length(L,N),
                   player_list(T,M),
                   N is M+1,
                   position(H,_),
                   \+ member(H,T).

team_config(L,N):- player_list(L,N),
                   N==11,
                   count_goalie(L,1),
                   count_defender(L,X),
                   X>=2,
                   forward_line(L,Y),
                   Y>=1.

locker_assignment([H],1).
locker_assignment(L,N):- player_list(L,N),
                         L=[H|T],
                         T=[H1|T1],
                         position(H,X),
                         position(H1,Y),
                         X \= Y,
                         M is N-1,
                         locker_assignment(T,M).

banquet_talk([],0).
banquet_talk([H],1).
banquet_talk(L,N):- player_list(L,N),
                    L=[H|T],
                    T=[H1|T1],
                    ((language(H,Y),language(H1,Y));(club(H,X),club(H1,X))),
                    M is N-1,
                    banquet_talk(T,M).

goalie(H):- position(H,goalie).
defender(H):- position(H,defender).
center(H):- position(H,center).
leftwinger(H):- position(H,leftwinger).
rightwinger(H):- position(H,rightwinger).

count_goalie([],0).
count_goalie([H|T],Y):- \+ goalie(H),count_goalie(T,Y).
count_goalie([H|T],X):- goalie(H),count_goalie(T,Y),X is Y+1 .

count_defender([],0).
count_defender([H|T],Y):- \+ defender(H),count_defender(T,Y).
count_defender([H|T],X):- defender(H),count_defender(T,Y),X is Y+1 .

count_center([],0).
count_center([H|T],Y):- \+ center(H),count_center(T,Y).
count_center([H|T],X):- center(H),count_center(T,Y),X is Y+1 .

count_leftwinger([],0).
count_leftwinger([H|T],Y):- \+ leftwinger(H),count_leftwinger(T,Y).
count_leftwinger([H|T],X):- leftwinger(H),count_leftwinger(T,Y),X is Y+1 .

count_rightwinger([],0).
count_rightwinger([H|T],Y):- \+ rightwinger(H),count_rightwinger(T,Y).
count_rightwinger([H|T],X):- rightwinger(H),count_rightwinger(T,Y),X is Y+1 .

forward_line(L,X):- player_list(L,_),
                    count_center(L,X),
                    count_leftwinger(L,X),
                    count_rightwinger(L,X).
