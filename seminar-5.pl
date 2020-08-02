:-use_module(library(clpfd)).

% toaster problem
% minimize the number of toaster usages.
% two toasts can be baked, but only from one side
all_lower([], _):-!.
all_lower([H|T], X):-
    H #< X,
    all_lower(T, X).

columns_different([], []):-!.
columns_different([H1|T1], [H2|T2]):-
    H1 #\= H2,
    columns_different(T1, T2).

zip([], [], []).
zip([H1|T1], [H2|T2], [[H1,H2]|T3]) :- zip(T1, T2, T3).

toaster(Toasts, Schedule):-
    length(S1, Toasts), length(S2, Toasts),
    append(S1, S2, S), append(S, [X], Vars),
    zip(S1, S2, Schedule),

    M is 2*Toasts,
    domain(S, 0, M),
    domain([X], Toasts, M),

    all_different(S1),
    all_different(S2),
    columns_different(S1, S2),
    all_lower(S, X),

    labeling([minimize(X)], Vars).
