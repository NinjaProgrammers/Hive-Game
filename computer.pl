
:- module(computer, [available_moves_white/1, available_moves_black/1]).
:- use_module([pieces,validate_moves,position,utils,adjacency]).

moves(B,S) :- 
        position(B,-1), !,
        color(B,C),
        insert_positions(C,S).
moves(B,S) :- 
        type(B,bee),
        position(B,P),
        update_position(B,-1),
        moves_bee(P,T),
        update_position(B,P),
        findall(X,(member(X,T),remain_connected(B,X)),S). 
moves(B,S) :- 
        type(B,beetle),
        position(B,P),
        update_position(B,-1),
        moves_beetle(P,T),
        update_position(B,P),
        findall(X,(member(X,T),remain_connected(B,X)),S). 
moves(B,S) :- 
        type(B,grasshoper),
        position(B,P),
        update_position(B,-1),
        moves_grasshoper(P,T),
        update_position(B,P),
        findall(X,(member(X,T),remain_connected(B,X)),S). 
moves(B,S) :- 
        type(B,spider),
        position(B,P),
        update_position(B,-1),
        moves_spider(P,T),
        update_position(B,P),
        findall(X,(member(X,T),remain_connected(B,X)),S). 
moves(B,S) :- 
        type(B,ant),
        position(B,P),
        update_position(B,-1),
        moves_ant(P,T),
        update_position(B,P),
        findall(X,(member(X,T),remain_connected(B,X)),S). 
  
available_moves(white,S) :- !, 
        available_moves_white(S).
available_moves(black,S) :- !,
        available_moves_black(S).

reformat(_,[],[]).
reformat(B,[X|S],[(B:X)|T]) :- reformat(B,S,T).
available_moves_white(S) :- 
        findall(X,(piece(X),color(X,white)),Y),
        available_moves_white_visit(Y,S), !.
available_moves_white_visit([],[]).
available_moves_white_visit([X|S],[(X:142)|T]) :-
        white_on_board(0),
        available_moves_white_visit(S,T), !.
available_moves_white_visit([X|S],P) :-
        position(white_bee,-1),
        position(X,-1), !,
        insert_positions(white,T),
        reformat(X,T,W),
        available_moves_white_visit(S,Q),
        append(W,Q,P).
available_moves_white_visit([X|S],T) :-
        position(white_bee,-1),
        position(X,P),
        P =\= -1, !,
        available_moves_white_visit(S,T).
available_moves_white_visit([X|S],T) :-
        position(X,P),
        not(upper_bug(P,X)), !,
        available_moves_white_visit(S,T).
available_moves_white_visit([X|P],S) :- 
        moves(X,T),
        reformat(X,T,W),
        available_moves_white_visit(P,Q),
        append(W,Q,S).

available_moves_black(S) :- 
        findall(X,(piece(X),color(X,black)),Y),
        available_moves_black_visit(Y,S), !.
available_moves_black_visit([X|S],[(X:141)|T]) :-
        black_on_board(0),
        available_moves_black_visit(S,T), !.
available_moves_black_visit([X|S],P) :-
        position(black_bee,-1),
        position(X,-1), !,
        insert_positions(black,T),
        reformat(X,T,W),
        available_moves_black_visit(S,Q),
        append(W,Q,P).
available_moves_black_visit([X|S],T) :-
        position(black_bee,-1),
        position(X,P),
        P =\= -1, !,
        available_moves_black_visit(S,T).
available_moves_black_visit([],[]).
available_moves_black_visit([X|S],T) :-
        position(X,P),
        not(upper_bug(P,X)), !,
        available_moves_black_visit(S,T).
available_moves_black_visit([X|P],S) :- 
        moves(X,T),
        reformat(X,T,W),
        available_moves_black_visit(P,Q),
        append(W,Q,S).

% Limite de profundidad del minimax
minimax_limit(3).

alphabeta(Depth,Alpha,Beta,BetterMove,Value) :-
        turn(C),
        minimax_limit(Lim),
        ((Depth < Lim, available_moves(C,L),not(length(L,0))) ->
        boundedbest(Depth,L,Alpha,Beta,BetterMove,Value);
        evaluate(Value)), !.
boundedbest(Depth,[(B:P)|L],Alpha,Beta,BetterMove,Value) :-
        move_piece(B,P),
        change_turn,
        D is Depth + 1,
        alphabeta(D,Alpha,Beta,_,V),
        undo_move,
        goodenough(Depth,L,Alpha,Beta,(B:P),V,BetterMove,Value).
goodenough(_,[],_,_,Pos,Val,Pos,Val) :- !.
goodenough(_,_,Alpha,Beta,Pos,Val,Pos,Val) :- 
        (turn(white), Val > Beta);
        (turn(black), Val < Alpha), !.
goodenough(Depth,L,Alpha,Beta,Pos,Val,BetterMove,Value) :-
        updatealphabeta(Alpha,Beta,NAlpha,NBeta),
        boundedbest(Depth,L,NAlpha,NBeta,P1,V1), 
        betterof(Pos,Val,P1,V1,BetterMove,Value).

betterof(Pos,Val,_,Val1,Pos,Val) :-
        (turn(white), Val > Val1);
        (turn(black), Val < Val1), !.
betterof(_,_,Pos1,Val1,Pos1,Val1).
updatealphabeta(Alpha, Beta, NAlpha, NBeta) :-
        At is Alpha + 2,
        Bt is Beta - 2,
        NAlpha is min(0, At),
        NBeta is max(0, Bt).


        
evaluate_pieces_out(R) :-
        findall(X,(piece(X),color(X,white),position(X,-1)),W),
        findall(X,(piece(X),color(X,black),position(X,-1)),B),
        length(W,P),
        length(B,Q),
        R is 15 * (Q - P).

evaluate_pieces_blocked(R) :-
        findall(X,(piece(X),color(X,white),not(position(X,-1)),
                moves(X,S),length(S,0)),W),
        findall(X,(piece(X),color(X,black),not(position(X,-1)),
                moves(X,S),length(S,0)),B),
        length(W,P),
        length(B,Q),
        R is 6 * (Q - P).

evaluate_neighbor_bee(R) :-  
        evaluate_neighbor_white_bee(P),
        evaluate_neighbor_black_bee(Q),
        %white_on_board(W),
        %black_on_board(B),
        R is (Q - P).
evaluate_neighbor_white_bee(R) :-
        position(white_bee,P),
        (P =:= -1 -> R is 0;
        (findall(X,(adjacent(P,X),occupied(X)),Y),
        length(Y,T)), R is T * T).
evaluate_neighbor_black_bee(R) :-
        position(black_bee,P),
        (P =:= -1 -> R is 0;
        (findall(X,(adjacent(P,X),occupied(X)),Y),
        length(Y,T)), R is T * T).

evaluate(0) :-
        draw, !.
evaluate(-1000) :-
        white_lose, !.
evaluate(1000) :-
        black_lose, !.
evaluate(R):- 
        evaluate_neighbor_white_bee(W),
        evaluate_neighbor_black_bee(B),
        W > B, W >= 4, !,
        R is -W * 100.
evaluate(R):- 
        evaluate_neighbor_white_bee(W),
        evaluate_neighbor_black_bee(B),
        W < B, B >= 4, !,
        R is B * 100.
evaluate(R) :- 
        evaluate_pieces_out(Po),
        evaluate_pieces_blocked(Pb),
        R is Po + Pb.
        





