:- module(position, [position/2, update_position/2, restart_positions/0,
                    last_in_stack/2, upper_bug/2, move_piece/2, undo_move/0,
                    counter/1, save_nomove/0]).

:- dynamic position/2, stack/2.

position(red_bee, -1).
position(red_beetle_1, -1).
position(red_beetle_2, -1).
position(red_grasshoper_1, -1).
position(red_grasshoper_2, -1).
position(red_grasshoper_3, -1).
position(red_spider_1, -1).
position(red_spider_2, -1).
position(red_ant_1, -1).
position(red_ant_2, -1).
position(red_ant_3, -1).

position(red_ladybug, -1).
position(red_mosquito, -1).
position(red_pillbug, -1).


position(blue_bee, -1).
position(blue_beetle_1, -1).
position(blue_beetle_2, -1).
position(blue_grasshoper_1, -1).
position(blue_grasshoper_2, -1).
position(blue_grasshoper_3, -1).
position(blue_spider_1, -1).
position(blue_spider_2, -1).
position(blue_ant_1, -1).
position(blue_ant_2, -1).
position(blue_ant_3, -1).

position(blue_ladybug, -1).
position(blue_mosquito, -1).
position(blue_pillbug, -1).


stack(red_bee, -1).
stack(red_beetle_1, -1).
stack(red_beetle_2, -1).
stack(red_grasshoper_1, -1).
stack(red_grasshoper_2, -1).
stack(red_grasshoper_3, -1).
stack(red_spider_1, -1).
stack(red_spider_2, -1).
stack(red_ant_1, -1).
stack(red_ant_2, -1).
stack(red_ant_3, -1).

stack(red_ladybug, -1).
stack(red_mosquito, -1).
stack(red_pillbug, -1).


stack(blue_bee, -1).
stack(blue_beetle_1, -1).
stack(blue_beetle_2, -1).
stack(blue_grasshoper_1, -1).
stack(blue_grasshoper_2, -1).
stack(blue_grasshoper_3, -1).
stack(blue_spider_1, -1).
stack(blue_spider_2, -1).
stack(blue_ant_1, -1).
stack(blue_ant_2, -1).
stack(blue_ant_3, -1).

stack(blue_ladybug, -1).
stack(blue_mosquito, -1).
stack(blue_pillbug, -1).


update_position(Piece, Pos) :- position(Piece, X),
                            Y =.. [position,Piece,X],
                            retract(Y),
                            Z =.. [position,Piece,Pos],
                            assert(Z).

:- use_module([pieces]).
restart_positions_1([]).
restart_positions_1([X|Y]) :- update_position(X,-1), restart_positions_1(Y).
restart_positions :- findall(X,piece(X),Y), restart_positions_1(Y), restart_counter.

update_stack(Piece) :- position(Piece,Pos), stack(Piece,S), 
        X =.. [stack,Piece,S], retract(X),
        last_in_stack(Pos,Y), Z is Y + 1,
        W =.. [stack,Piece,Z], assert(W).

max_or_default([],-1).
max_or_default(X,Y) :- max_list(X,Y).
last_in_stack(Pos,Y) :- findall(N,(position(P,Pos),stack(P,N)),S), 
        max_or_default(S,Y).

upper_bug_1([(X:M)|_],M,X) :- !.
upper_bug_1([_|S],M,Y) :- upper_bug_1(S,M,Y).
upper_bug(Pos,B) :- findall((X:Y), (position(X,Pos),stack(X,Y)),S),
        findall(N,(position(P,Pos),stack(P,N)),T), max_list(T,M),
        upper_bug_1(S,M,B).

:- dynamic counter/1.
counter(0).
increment_counter :- 
        counter(C), X =.. [counter,C], retract(X),
        Y is C + 1, Z =.. [counter,Y], assert(Z).
decrement_counter :- 
        counter(C), X =.. [counter,C], retract(X),
        Y is C - 1, Z =.. [counter,Y], assert(Z).

restart_counter_visit :- 
        moves_record(C,B,S,E), !,
        X =.. [moves_record,C,B,S,E],
        retract(X),
        restart_counter_visit.
restart_counter_visit.
restart_counter :-  
        restart_counter_visit,
        counter(C), C =\= 0, !,
        X =.. [counter,C], retract(X),
        Z =.. [counter,0], assert(Z).
restart_counter.


:- use_module([utils]).

:- dynamic moves_record/4.
save_nomove :- 
        increment_counter,
        counter(C), 
        X =.. [moves_record,C,-2,-2,-2],
        assert(X). 
save_move(B,S,E) :- 
        increment_counter,
        counter(C), 
        X =.. [moves_record,C,B,S,E],
        assert(X).
undo_move :- 
        counter(C),
        moves_record(C,B,S,E),
        X =.. [moves_record,C,B,S,E],
        retract(X),
        decrement_counter,
        (S =\= -2 -> 
        (update_position(B,S),
        update_stack(B))),
        change_turn.

move_piece(Piece,Pos) :- 
        position(Piece,S),
        update_position(Piece,Pos), 
        update_stack(Piece),
        save_move(Piece,S,Pos).


