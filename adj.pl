adjacency(X,1,Y):- Z is X mod 15, Z =:= 0 -> Y is -1; Y is X - 1.

adjacency(X,2,-1):- X >= 285.
adjacency(X,2,-1):- X < 285, Z is X mod 30, Z =:= 0.
adjacency(X,2,Y):- X < 285, Z is X mod 30, Z > 0, Z < 15, Y is X + 14.
adjacency(X,2,Y):- X < 285, Z is X mod 30, Z >= 15, Y is X + 15.

adjacency(X,3,-1):- X >= 285.
adjacency(X,3,-1):- X < 285, Z is X mod 30, Z =:= 29.
adjacency(X,3,Y):- X < 285, Z is X mod 30, Z < 15, Y is X + 15.
adjacency(X,3,Y):- X < 285, Z is X mod 30, Z >= 15, Z < 29, Y is X + 16.

adjacency(X,4,Y):- Z is X mod 15, Z =:= 14 -> Y is -1; Y is X + 1.

adjacency(X,5,-1):- X < 15.
adjacency(X,5,-1):- X >= 15, Z is X mod 30, Z =:= 29.
adjacency(X,5,Y):- X >= 15, Z is X mod 30, Z < 15, Y is X - 15.
adjacency(X,5,Y):- X > 15, Z is X mod 30, Z >= 15, Z < 29, Y is X - 14.

adjacency(X,6,-1):- X < 15.
adjacency(X,6,-1):- X >= 15, Z is X mod 30, Z =:= 0.
adjacency(X,6,Y):- X >= 15, Z is X mod 30, Z > 0, Z < 15, Y is X - 16.
adjacency(X,6,Y):- X >= 15, Z is X mod 30, Z >= 15, Y is X - 15.


a(300) :- !.
a(C) :- C < 113,
    X is C + 1,
    b(C, 1),
    a(X).

b(_,7) :- !.
b(C,D) :- D < 7,
    L is D + 1,
    adjacency(C,D,-1), !,
    b(C,L).

b(C,D) :- D < 7,
    L is D + 1,
    adjacency(C,D,P),
    write("adjacency("), write(C), write(","),write(D),write(","),write(P),writeln(")."),
    b(C,L).



