:-use_module(library(clpfd)).

no_zero([]):-!.
no_zero([H|T]):-
    H #\= 0,
    no_zero(T).

seats2(_, []):-!.
seats2(X, [H|T]):-
    abs(H-X) #>= 2,
    seats2(X, T).

seats([]):-!.
seats([H|T]):-
    seats2(H, T),
    seats(T).

weighted_sum([], [], 0).
weighted_sum([H1|T1], [H2|T2], X):-
    weighted_sum(T1, T2, X1),
    X #= X1 + H1 * H2.

boys(Weights, Seats, Bois):-
    length(Weights, N),
    length(Bois, N),

    MSeats is -1 * Seats,
    domain(Bois, MSeats, Seats),

    no_zero(Bois),
    seats(Bois),

    weighted_sum(Bois, Weights, X),
    Y #= abs(X),

    labeling([minimize(Y)], Bois).


