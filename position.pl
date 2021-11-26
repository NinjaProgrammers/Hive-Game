:- module(position, [position/2, update_position/2, restart_positions/0,
                    last_in_stack/2, upper_bug/2, move_piece/2, undo_move/0,
                    counter/1]).

:- dynamic position/2, stack/2.

position(white_bee, -1).
position(white_beetle_1, -1).
position(white_beetle_2, -1).
position(white_grasshoper_1, -1).
position(white_grasshoper_2, -1).
position(white_grasshoper_3, -1).
position(white_spider_1, -1).
position(white_spider_2, -1).
position(white_ant_1, -1).
position(white_ant_2, -1).
position(white_ant_3, -1).

position(white_ladybug, -1).
position(white_mosquito, -1).
position(white_pillbug, -1).


position(black_bee, -1).
position(black_beetle_1, -1).
position(black_beetle_2, -1).
position(black_grasshoper_1, -1).
position(black_grasshoper_2, -1).
position(black_grasshoper_3, -1).
position(black_spider_1, -1).
position(black_spider_2, -1).
position(black_ant_1, -1).
position(black_ant_2, -1).
position(black_ant_3, -1).

position(black_ladybug, -1).
position(black_mosquito, -1).
position(black_pillbug, -1).


stack(white_bee, -1).
stack(white_beetle_1, -1).
stack(white_beetle_2, -1).
stack(white_grasshoper_1, -1).
stack(white_grasshoper_2, -1).
stack(white_grasshoper_3, -1).
stack(white_spider_1, -1).
stack(white_spider_2, -1).
stack(white_ant_1, -1).
stack(white_ant_2, -1).
stack(white_ant_3, -1).

stack(white_ladybug, -1).
stack(white_mosquito, -1).
stack(white_pillbug, -1).


stack(black_bee, -1).
stack(black_beetle_1, -1).
stack(black_beetle_2, -1).
stack(black_grasshoper_1, -1).
stack(black_grasshoper_2, -1).
stack(black_grasshoper_3, -1).
stack(black_spider_1, -1).
stack(black_spider_2, -1).
stack(black_ant_1, -1).
stack(black_ant_2, -1).
stack(black_ant_3, -1).

stack(black_ladybug, -1).
stack(black_mosquito, -1).
stack(black_pillbug, -1).


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
restart_counter :- 
        counter(0), !.
restart_counter :-  
        decrement_counter,
        counter(C),
        moves_record(C,B,S,E),
        X =.. [moves_record,C,B,S,E],
        retract(X),
        restart_counter.

:- use_module([utils]).

:- dynamic moves_record/4.
save_move(B,S,E) :- 
        counter(C), 
        X =.. [moves_record,C,B,S,E],
        assert(X),
        increment_counter.
undo_move :- 
        decrement_counter,
        counter(C),
        moves_record(C,B,S,E),
        X =.. [moves_record,C,B,S,E],
        retract(X),
        update_position(B,S),
        update_stack(B),
        change_turn.

move_piece(Piece,Pos) :- 
        position(Piece,S),
        update_position(Piece,Pos), 
        update_stack(Piece),
        save_move(Piece,S,Pos).


