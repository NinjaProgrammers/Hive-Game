:- use_module([position,utils,output_facilities, pieces, validate_moves, computer]).

:- op(1100, fx, move).
:- op(1000, xfy, to).
move X to Y :- move_1(X,Y), !.

move_1(X,Y) :- 
    color(X,C), 
    turn(CC), 
    (C == CC ->
    move_2(X,Y) ;
    (write("It's "), write(CC), writeln(" turn."))).
move_2(X,Y) :- 
    white_on_board(0) ->
    (move_piece(X,142), move_7);
    move_3(X,Y).
move_3(X,Y) :- 
    black_on_board(0) -> 
    (move_piece(X,141), move_7);
    move_4(X,Y). 
move_4(X,Y) :-
    turn(white),
    ((not(X == white_bee), position(white_bee,-1), counter(6)) ->
    writeln("You have to move your queen in the first four steps.");
    move_5(X,Y)).
move_4(X,Y) :-
    turn(black),
    ((not(X == black_bee), position(black_bee,-1), counter(7)) ->
    writeln("You have to move your queen in the first four steps.");
    move_5(X,Y)).
move_5(X,Y) :-
    turn(white),
    ((not(X == white_bee), position(white_bee,-1), position(X,P), P =\= -1) ->
    writeln("You have to move your queen before moving any other piece in the board.");
    move_6(X,Y)).
move_5(X,Y) :-
    turn(black),
    ((not(X == black_bee), position(black_bee,-1), position(X,P), P =\= -1) ->
    writeln("You have to move your queen before moving any other piece in the board.");
    move_6(X,Y)).
move_6(X,Y) :-
    validate_move(X,Y) ->
    (move_piece(X,Y), move_7) ;
    writeln("Invalid move.").
move_7 :-
    game_ended ->
    play_hive;
    (change_turn, move_8).
move_8 :- 
    turn(white), 
    white_on_board(W),
    W > 0,
    available_moves_white(L),
    length(L,0),
    writeln("White does not have available moves."),
    save_nomove,
    change_turn, !.
move_8 :- 
    turn(black), 
    black_on_board(B),
    B > 0,
    available_moves_black(L),
    length(L,0),
    writeln("Black does not have available moves."),
    save_nomove,
    change_turn, !.
move_8.

    

game_ended :- draw, !, writeln("Draw.").
game_ended :- white_lose, !, writeln("Black won.").
game_ended :- black_lose, !, writeln("White won.").
game_ended :- print_board, fail.

play_hive :- restart_positions, change_turn_white.


:- op(1000, fx, best_move).

best_move X :- 
        I is -X,
        alphabeta(3,I,X,(B:P),_),
        write("Moving "), write(B), write(" to "),
        write(P), writeln("."),
        (move B to P).