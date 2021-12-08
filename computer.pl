
:- module(computer, [available_moves_red/1, available_moves_blue/1,
        alphabeta/5]).
:- use_module([pieces,validate_moves,position,utils,adjacency]).

moves(B,S) :-
        position(B,-1), !,
        color(B,C),
        insert_positions(C,S).
moves(B,[]) :-
        position(B,P),
        update_position(B,-1),
        connected_components(CC),
        update_position(B,P),
        length(CC,L),
        L > 1, !.
moves(B,S) :-
        type(B,bee),!,
        position(B,P),
        update_position(B,-1),
        moves_bee(P,T),
        update_position(B,P),
        findall(X,(member(X,T),remain_connected(B,X)),S).
moves(B,S) :-
        type(B,beetle),!,
        position(B,P),
        update_position(B,-1),
        moves_beetle(P,T),
        update_position(B,P),
        findall(X,(member(X,T),remain_connected(B,X)),S).
moves(B,S) :-
        type(B,grasshoper),!,
        position(B,P),
        update_position(B,-1),
        moves_grasshoper(P,T),
        update_position(B,P),
        findall(X,(member(X,T),remain_connected(B,X)),S).
moves(B,S) :-
        type(B,spider),!,
        position(B,P),
        update_position(B,-1),
        moves_spider(P,T),
        update_position(B,P),
        findall(X,(member(X,T),remain_connected(B,X)),S).
moves(B,S) :-
        type(B,ant),!,
        position(B,P),
        update_position(B,-1),
        moves_ant(P,T),
        update_position(B,P),
        findall(X,(member(X,T),remain_connected(B,X)),S).

moves(B,S) :-
        type(B,ladybug),!,
        position(B,P),
        update_position(B,-1),
        moves_ladybug(P,T),
        update_position(B,P),
        findall(X,(member(X,T),remain_connected(B,X)),S).

moves(B,S) :-
        type(B,mosquito),!,
        position(B,P),
        update_position(B,-1),
        moves_mosquito(P,T),
        update_position(B,P),
        findall(X,(member(X,T),remain_connected(B,X)),S).

available_moves(red,S) :-
        counter(6),
        position(red_bee,-1), !,
        insert_positions(red,L),
        reformat(red_bee,L,S).
available_moves(red,S) :- !,
        available_moves_red(S).
available_moves(blue,S) :-
        counter(7),
        position(blue_bee,-1), !,
        insert_positions(blue,L),
        reformat(blue_bee,L,S).
available_moves(blue,S) :- !,
        available_moves_blue(S).

reformat(_,[],[]).
reformat(B,[X|S],[(B:X)|T]) :- reformat(B,S,T).
multi_reformat([X|S],L,T) :-
        reformat(X,L,A),
        multi_reformat(S,L,B),
        append(A,B,T).
multi_reformat([],_,[]).

filter_by_type([Cur|R],Types,[Cur|S]) :-
        type(Cur,T),
        not(member(T,Types)), !,
        append([T],Types,NT),
        filter_by_type(R,NT,S).
filter_by_type([Cur|R],Types,S) :-
        type(Cur,T),
        member(T,Types), !,
        filter_by_type(R,Types,S).
filter_by_type([],_,[]).

first_move_red([],[]).
first_move_red([B|S],[(B:142)|T]) :-
        first_move_red(S,T).
available_moves_red(S) :-
        red_on_board(0), !,
        findall(X,(piece(X),color(X,red)),Y),
        first_move_red(Y, S).
available_moves_red(S) :-
        findall(X,(piece(X),color(X,red),position(X,-1)),Z),
        filter_by_type(Z,[],NZ),
        insert_positions(red,T),
        multi_reformat(NZ,T,Q),
        findall(X,(piece(X),color(X,red),position(X,P),P =\= -1),Y),
        available_moves_red_visit(Y,W),
        append(Q,W,S), !.
available_moves_red_visit([],[]).
available_moves_red_visit(_,[]) :-
        position(red_bee,-1), !.
available_moves_red_visit([X|S],T) :-
        position(X,P),
        P =\= -1,
        not(upper_bug(P,X)), !,
        available_moves_red_visit(S,T).
available_moves_red_visit([X|P],S) :-
        moves(X,T),
        reformat(X,T,W),
        available_moves_red_visit(P,Q),
        append(W,Q,S).

first_move_blue([],[]).
first_move_blue([B|S],[(B:141)|T]) :-
        first_move_blue(S,T).
available_moves_blue(S) :-
        blue_on_board(0), !,
        findall(X,(piece(X),color(X,blue)),Y),
        first_move_blue(Y, S).
available_moves_blue(S) :-
        findall(X,(piece(X),color(X,blue),position(X,-1)),Z),
        filter_by_type(Z,[],NZ),
        insert_positions(blue,T),
        multi_reformat(NZ,T,Q),
        findall(X,(piece(X),color(X,blue),position(X,P),P =\= -1),Y),
        available_moves_blue_visit(Y,W),
        append(Q,W,S), !.
available_moves_blue_visit(_,[]) :-
        position(blue_bee,-1), !.
available_moves_blue_visit([],[]).
available_moves_blue_visit([X|S],T) :-
        position(X,P),
        P =\= -1,
        not(upper_bug(P,X)), !,
        available_moves_blue_visit(S,T).
available_moves_blue_visit([X|P],S) :-
        moves(X,T),
        reformat(X,T,W),
        available_moves_blue_visit(P,Q),
        append(W,Q,S).

alphabeta(Depth,Alpha,Beta,BetterMove,Value) :-
        turn(C),
        ((Depth > 0, available_moves(C,L),not(length(L,0))) ->
        boundedbest(Depth,L,Alpha,Beta,BetterMove,Value);
        evaluate(Value)), !.
boundedbest(Depth,[(B:P)|L],Alpha,Beta,BetterMove,Value) :-
        move_piece(B,P),
        change_turn,
        D is Depth - 1,
        alphabeta(D,Alpha,Beta,_,V),
        undo_move,
        goodenough(Depth,L,Alpha,Beta,(B:P),V,BetterMove,Value).
goodenough(_,[],_,_,Pos,Val,Pos,Val) :- !.
goodenough(_,_,Alpha,Beta,Pos,Val,Pos,Val) :-
        %write("Val"),write(Val),write("Alpha"), write(Alpha),write("Beta"),write(Beta),write("\n"),
        (turn(red), Val >= Beta), !;
        (turn(blue), Val =< Alpha), !.
goodenough(Depth,L,Alpha,Beta,Pos,Val,BetterMove,Value) :-
        %write(Val), write(Alpha),write(Beta),write("\n"),
        updatealphabeta(Alpha,Beta,NAlpha,NBeta),
        boundedbest(Depth,L,NAlpha,NBeta,P1,V1),
        betterof(Pos,Val,P1,V1,BetterMove,Value).

betterof(Pos,Val,_,Val1,Pos,Val) :-
        (turn(red), Val > Val1), !;
        (turn(blue), Val < Val1), !.
betterof(_,_,Pos1,Val1,Pos1,Val1).
updatealphabeta(Alpha, Beta, NAlpha, NBeta) :-
        At is Alpha + 2,
        Bt is Beta - 2,
        NAlpha is min(-4, At),
        NBeta is max(4, Bt).



evaluate_pieces_out(R) :-
        findall(X,(piece(X),color(X,red),position(X,-1)),W),
        findall(X,(piece(X),color(X,blue),position(X,-1)),B),
        length(W,P),
        length(B,Q),
        R is 3 * (Q - P).

evaluate_pieces_blocked(R) :-
        findall(X,(piece(X),color(X,red),not(position(X,-1)),
                moves(X,S),length(S,0)),W),
        findall(X,(piece(X),color(X,blue),not(position(X,-1)),
                moves(X,S),length(S,0)),B),
        length(W,P),
        length(B,Q),
        R is 3 * (Q - P).

evaluate_neighbor_bee(R) :-
        evaluate_neighbor_red_bee(P),
        evaluate_neighbor_blue_bee(Q),
        %red_on_board(W),
        %blue_on_board(B),
        R is (Q - P).
evaluate_neighbor_red_bee(R) :-
        position(red_bee,P),
        (P =:= -1 -> R is 0;
        (findall(X,(adjacent(P,X),occupied(X)),Y),
        length(Y,T)), R is T * T * T).
evaluate_neighbor_blue_bee(R) :-
        position(blue_bee,P),
        (P =:= -1 -> R is 0;
        (findall(X,(adjacent(P,X),occupied(X)),Y),
        length(Y,T)), R is T * T * T).

greater(W,B,R) :-
        W > B ->
        R is W * -1;
        R is B * 1.
evaluate(R):-
        ((red_lose -> X is -50; X is 0),
        (blue_lose -> Y is 50; Y is 0)),
        R is X + Y.
evaluate(R):-
        evaluate_neighbor_red_bee(W),
        evaluate_neighbor_blue_bee(B),
        R is B - W.
evaluate(R) :-
        evaluate_pieces_out(Po),
        %evaluate_pieces_blocked(Pb),
        R is Po.


