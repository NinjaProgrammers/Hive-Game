:- use_module([position,utils,output_facilities, pieces, validate_moves, computer]).

:- op(1100, fx, move).
:- op(1000, xfy, to).
move X to Y :- move_1(X,Y), !, move_computer.

move_1(X,Y) :-
    color(X,C),
    turn(CC),
    (C == CC ->
    move_2(X,Y) ;
    (write("It's "), write(CC), writeln(" turn."))).
move_2(X,Y) :-
    red_on_board(0) ->
    (move_piece(X,142), move_7);
    move_3(X,Y).
move_3(X,Y) :-
    blue_on_board(0) ->
    (move_piece(X,141), move_7);
    move_4(X,Y).
move_4(X,Y) :-
    turn(red),
    ((not(X == red_bee), position(red_bee,-1), counter(6)) ->
    writeln("You have to move your queen in the first four steps.");
    move_5(X,Y)).
move_4(X,Y) :-
    turn(blue),
    ((not(X == blue_bee), position(blue_bee,-1), counter(7)) ->
    writeln("You have to move your queen in the first four steps.");
    move_5(X,Y)).
move_5(X,Y) :-
    turn(red),
    ((not(X == red_bee), position(red_bee,-1), position(X,P), P =\= -1) ->
    writeln("You have to move your queen before moving any other piece in the board.");
    move_6(X,Y)).
move_5(X,Y) :-
    turn(blue),
    ((not(X == blue_bee), position(blue_bee,-1), position(X,P), P =\= -1) ->
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
    turn(red),
    red_on_board(W),
    W > 0,
    available_moves_red(L),
    length(L,0),
    writeln("Red does not have available moves."),
    save_nomove,
    change_turn, !.
move_8 :-
    turn(blue),
    blue_on_board(B),
    B > 0,
    available_moves_blue(L),
    length(L,0),
    writeln("Blue does not have available moves."),
    save_nomove,
    change_turn, !.
move_8.



game_ended :- draw, !, print_board, writeln("Draw.").
game_ended :- red_lose, !, print_board, writeln("Blue won.").
game_ended :- blue_lose, !, print_board, writeln("Red won.").
game_ended :- print_board, fail.

play_hive :- restart_positions, change_turn_red.


:- op(1000, fx, best_move).

best_move X :-
        I is -X,
        alphabeta(3,I,X,(B:P),_),
        write("Moving "), write(B), write(" to "),
        write(P), writeln("."),
        (move B to P).

hint:-
     alphabeta(2,-10,10,(B:P),_),
     write("Move "), write(B), write(" to "),
     write(P), writeln(".").


:- dynamic is_ia_activated/0.
activate_ia :-
    not(is_ia_activated),
    X =.. [is_ia_activated],
    assert(X),
    turn(blue),
    move_computer.
deactivate_ia :-
    is_ia_activated,
    X =.. [is_ia_activated],
    retract(X).


move_computer:-
    turn(blue),
    is_ia_activated,
    best_move(10), !.
move_computer:-
    turn(red), !.


activate_red_ladybug :-
    not(piece(red_ladybug)),
    X =.. [piece,red_ladybug],
    assert(X).
activate_blue_ladybug :-
    not(piece(blue_ladybug)),
    X =.. [piece,blue_ladybug],
    assert(X).
activate_ladybugs :-
    activate_red_ladybug,
    activate_blue_ladybug,
    play_hive,
    writeln("New game started.").

deactivate_red_ladybug :-
    piece(red_ladybug),
    X =.. [piece,red_ladybug],
    retract(X).
deactivate_blue_ladybug :-
    piece(blue_ladybug),
    X =.. [piece,blue_ladybug],
    retract(X).
deactivate_ladybugs :-
    play_hive,
    deactivate_red_ladybug,
    deactivate_blue_ladybug,
    writeln("New game started.").


activate_red_mosquito :-
    not(piece(red_mosquito)),
    X =.. [piece,red_mosquito],
    assert(X).
activate_blue_mosquito :-
    not(piece(blue_mosquito)),
    X =.. [piece,blue_mosquito],
    assert(X).
activate_mosquitos :-
    activate_red_mosquito,
    activate_blue_mosquito,
    play_hive,
    writeln("New game started.").


deactivate_red_mosquito :-
    piece(red_mosquito),
    X =.. [piece,red_mosquito],
    retract(X).
deactivate_blue_mosquito :-
    piece(blue_mosquito),
    X =.. [piece,blue_mosquito],
    retract(X).
deactivate_mosquitos :-
    play_hive,
    deactivate_red_mosquito,
    deactivate_blue_mosquito,
    writeln("New game started.").

