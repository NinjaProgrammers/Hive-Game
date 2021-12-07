
:- module(validate_moves, [validate_move/2, moves_bee/2, insert_positions/2,
        moves_beetle/2, moves_grasshoper/2, moves_spider/2,moves_ant/2,isnt_articulation/1,
        remain_connected/2, moves_ladybug/2]).
:- use_module([adjacency, utils, pieces, position]).

moves_bee_visit(_,7,[]) :- !.
moves_bee_visit(Start,D,[P|X]) :-
        D < 7,
        adjacency(Start,D,P),
        P =\= -1,
        ground_move(Start,P),
        adjacent_to_hive(P), !,
        T is D + 1,
        moves_bee_visit(Start,T,X).
moves_bee_visit(Start,D,S) :-
        D < 7,
        T is D + 1,
        moves_bee_visit(Start,T,S).
moves_bee(Start,S) :- 
        moves_bee_visit(Start,1,S).

moves_beetle_visit(_,7,[]) :- !.
moves_beetle_visit(Start,D,S) :-
        adjacency(Start,D,-1), !,
        X is D + 1,
        moves_beetle_visit(Start,X,S).
moves_beetle_visit(Start,D,[P|S]) :-
        adjacency(Start,D,P), !,
        X is D + 1,
        moves_beetle_visit(Start,X,S).
moves_beetle(Start,S) :- moves_beetle_visit(Start,1,S).

moves_grasshoper_1(Start,Start,D,S) :- 
        adjacency(Start,D,X), 
        X =\= -1,
        occupied(X),
        moves_grasshoper_1(Start,X,D,S), 
        !.
moves_grasshoper_1(Start,Cur,D,S) :- 
        Start =\= Cur, 
        occupied(Cur), 
        adjacency(Cur,D,X),
        X =\= -1,
        moves_grasshoper_1(Start,X,D,S), 
        !.
moves_grasshoper_1(Start,Cur,_,[Cur]) :- 
        Start =\= Cur, 
        not(occupied(Cur)), 
        !.
moves_grasshoper_1(_,_,_,[]).
moves_grasshoper(Start,S) :- 
        moves_grasshoper_1(Start,Start,1,S1),
        moves_grasshoper_1(Start,Start,2,S2), 
        moves_grasshoper_1(Start,Start,3,S3),
        moves_grasshoper_1(Start,Start,4,S4),
        moves_grasshoper_1(Start,Start,5,S5),
        moves_grasshoper_1(Start,Start,6,S6),
        append([S1,S2,S3,S4,S5,S6],S). 
        
moves_spider_1(Seen,Queue,Depth,S) :- 
        Depth<3, !, 
        filter(Queue,adjacent_to_hive,Arr),
        append(Seen,Arr,T), 
        findall(X,(member(Y,Arr),adjacent(Y,X),
                ground_move(Y,X), not(member(X,T))),Next),
        N is Depth + 1,
        moves_spider_1(T,Next,N,S).
moves_spider_1(_,Queue,3,S) :- filter(Queue,adjacent_to_hive,S).
moves_spider(Start,S) :- moves_spider_1([],[Start],0,S).

dfs_ant(Seen,Cur,S) :- 
        findall(X,(adjacent(Cur,X),ground_move(Cur,X),adjacent_to_hive(X)),Y),
        dfs_ant_visit(Seen,Y,S).
dfs_ant_visit(Seen,[],Seen).
dfs_ant_visit(Seen,[X|Y],S) :- 
        member(X,Seen) -> 
        dfs_ant_visit(Seen,Y,S);
        (append([X],Seen,NS),
        dfs_ant(NS,X,T), 
        dfs_ant_visit(T,Y,S)).
moves_ant(Start,S) :- 
        dfs_empty([Start],Start,T), 
        delete(T,Start,S).
% tengo que kitarl la hormiga pa que esto funcione

moves_ladybug_1(Seen,Queue,Depth,S) :- 
        Depth<2, !, 
        append(Seen,Queue,T), 
        findall(X,(member(Y,Queue), adjacent(Y,X),
                occupied(X), not(member(X,T))),Next),
        N is Depth + 1,
        moves_ladybug_1(T,Next,N,S).
moves_ladybug_1(Seen,Queue,Depth,S) :- 
        Depth =:= 2, !, 
        append(Seen,Queue,T), 
        findall(X,(member(Y,Queue),adjacent(Y,X),
                not(occupied(X)), not(member(X,T))),S).
moves_ladybug(Start,S) :- moves_ladybug_1([],[Start],0,S).

validate_insert(_,[]).
validate_insert(Color,[X|S]) :- 
        upper_bug(X,B), 
        color(B,Color),
        validate_insert(Color,S).
insert_positions(Color,S) :-
        findall(X,(color(B,Color),position(B,P),adjacent(P,X),not(occupied(X))),Y),
        insert_positions_visit(Color,Y,S).
insert_positions_visit(Color,[Cur|R],[Cur|S]) :- 
        not(member(Cur,R)),
        findall(X,(adjacent(Cur,X),occupied(X)),Y),
        length(Y,Z), 
        Z > 0,
        validate_insert(Color,Y), !,
        insert_positions_visit(Color,R,S).
insert_positions_visit(Color,[_|R],S) :-
        insert_positions_visit(Color,R,S).
insert_positions_visit(_,[],[]).

% insert_positions_visit(Color,Cur,[Cur|S]) :- 
%         Cur < 300,
%         not(occupied(Cur)), 
%         findall(X,(adjacent(Cur,X),occupied(X)),Y),
%         length(Y,Z), 
%         Z > 0,
%         validate_insert(Color,Y), !,
%         C is Cur + 1,
%         insert_positions_visit(Color,C,S).
% insert_positions_visit(Color,Cur,S) :-
%         Cur < 300,
%         C is Cur + 1,
%         insert_positions_visit(Color,C,S).
% insert_positions_visit(_,300,[]).
% insert_positions(Color,S) :- insert_positions_visit(Color,0,S).

:- dynamic piece_position/1.
piece_position(-1).
set_position(X) :- 
        piece_position(Y),
        Z =.. [piece_position,Y],
        retract(Z),
        W =.. [piece_position,X],
        assert(W).

validate_move(B,End) :-
        position(B,P),
        (P =:= -1 ; upper_bug(P,B)),
        isnt_articulation(B),
        validate(B,P,End),
        remain_connected(B,End).

validate(B,-1,End) :- !, 
        color(B,Color),
        findall(X,(adjacent(End,X),occupied(X)),Y),
        length(Y,Z), 
        Z > 0,
        validate_insert(Color,Y).
validate(B,Start,End) :- 
        type(B,bee),
        validate_move_bee(Start,End).
validate(B,Start,End) :- 
        type(B,beetle),
        validate_move_beetle(Start,End).
validate(B,Start,End) :- 
        type(B,grasshoper),
        validate_move_grasshoper(Start,End).
validate(B,Start,End) :- 
        type(B,spider),
        validate_move_spider(Start,End).
validate(B,Start,End) :- 
        type(B,ant),
        validate_move_ant(Start,End).
validate(B,Start,End) :- 
        type(B,ladybug),
        validate_move_ladybug(Start,End).


validate_move_bee(Start,End) :- moves_bee(Start,S), member(End,S).

validate_move_beetle(Start,End) :- moves_beetle(Start,S), member(End,S).

validate_move_grasshoper(Start,End) :- moves_grasshoper(Start,S), member(End,S).

validate_move_spider(Start,End) :- moves_spider(Start,S), member(End,S).

validate_move_ant(Start,End) :- moves_ant(Start,S), member(End,S).

validate_move_ladybug(Start,End) :- moves_ladybug(Start,S), member(End,S).


isnt_articulation(B) :-
        position(B,P),
        set_position(P),
        update_position(B,-1),
        connected_components(CC),
        length(CC,1),
        update_position(B,P), !.
isnt_articulation(B) :- 
        piece_position(X),
        update_position(B,X),
        fail.

remain_connected(B,End) :- 
        position(B,P),
        set_position(P),
        update_position(B,End),
        connected_components(CC),
        length(CC,1),
        update_position(B,P).
remain_connected(B,_) :- 
        piece_position(X),
        update_position(B,X),
        fail.





