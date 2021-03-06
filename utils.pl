:- module(utils, [filter/3, ground_move/2, dfs_empty/3, occupied/1,
        draw/0, blue_lose/0, red_lose/0, connected_components/1,
        red_on_board/1, blue_on_board/1,turn/1,change_turn/0,change_turn_red/0,
        change_turn_blue/0,adjacent_to_hive/1,unique/2]).
:- use_module([adjacency, position, pieces]).

occupied(Position) :- position(_,Position), !.

filter([],_,[]).
filter([X|S],P,T) :-
        Z =.. [P,X], call(Z),
        !,
        filter(S,P,Y),
        append([X],Y,T).
filter([_|S],P,T) :- filter(S,P,T).

adjacent_to_hive(X) :-
        adjacent(X,Y),
        occupied(Y).

ground_move(_,End) :-
        occupied(End),
        !,
        fail.
ground_move(Start,End) :-
        adjacency(Start,D,End),
        Prev is 6 - (6 - D + 1) mod 6,
        adjacency(Start,Prev,P),
        occupied(P),
        Next is D mod 6 + 1,
        adjacency(Start,Next,N),
        occupied(N),
        !,
        fail.
ground_move(_,_).

dfs_empty(Seen,Cur,S) :-
        findall(X,(adjacent(Cur,X),ground_move(Cur,X)),Y),
        dfs_empty_visit(Seen,Y,S).
dfs_empty_visit(Seen,[],Seen).
dfs_empty_visit(Seen,[X|Y],S) :-
        member(X,Seen) ->
        dfs_empty_visit(Seen,Y,S);
        (append([X],Seen,NS),
        dfs_empty(NS,X,T),
        dfs_empty_visit(T,Y,S)).

dfs_occupied(Seen,Cur,S) :-
        append([Cur],Seen,NS),
        findall(X,(adjacent(Cur,X),occupied(X)),Y),
        dfs_occupied_visit(NS,Y,S).
dfs_occupied_visit(Seen,[],Seen).
dfs_occupied_visit(Seen,[X|Y],S) :-
        member(X,Seen) ->
        dfs_occupied_visit(Seen,Y,S);
        (dfs_occupied(Seen,X,T),
        dfs_occupied_visit(T,Y,S)).

connected_components(S) :- connected_components_visit([],0,S).
connected_components_visit(_,300,[]).
connected_components_visit(Seen,Pos,S) :-
        Pos < 300,
        (member(Pos,Seen) ; not(occupied(Pos))),
        !,
        P is Pos + 1,
        connected_components_visit(Seen,P,S).
connected_components_visit(Seen,Pos,S) :-
        Pos < 300,
        dfs_occupied([],Pos,X),
        append(X,Seen,NS),
        P is Pos + 1,
        connected_components_visit(NS,P,T),
        append([X],T,S).


blue_lose :-
        position(blue_bee,X),
        findall(Y,(adjacency(X,_,Y),occupied(Y),Y =\= -1),S),
        length(S,6).
red_lose :-
        position(red_bee,X),
        findall(Y,(adjacency(X,_,Y),occupied(Y),Y =\= -1),S),
        length(S,6).
draw :- blue_lose, red_lose.

on_board(X) :- position(X,Y), Y > -1.
red_on_board(N) :-
        findall(X,(piece(X),color(X,red)),Z),
        filter(Z,on_board,S),
        length(S,N).
blue_on_board(N) :-
        findall(X,(piece(X),color(X,blue)),Z),
        filter(Z,on_board,S),
        length(S,N).

:- dynamic turn/1.
turn(red).
change_turn :- turn(X), X == blue, change_turn_red, !.
change_turn :- turn(X), X == red, change_turn_blue.
change_turn_red :- turn(X), P =.. [turn,X], retract(P),
                     Q =.. [turn,red], assert(Q).
change_turn_blue :- turn(X), P =.. [turn,X], retract(P),
                     Q =.. [turn,blue], assert(Q).

unique_visit([Cur|R],V,[Cur|S]) :-
        not(member(Cur,V)), !,
        append([Cur],V,NV),
        unique_visit(R,NV,S).
unique_visit([Cur|R],V,S) :-
        member(Cur,V), !,
        unique_visit(R,V,S).
unique_visit([],_,[]).
unique(L,S) :-
        unique_visit(L,[],S).

