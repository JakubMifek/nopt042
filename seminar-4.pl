:-use_module(library(clpfd)).

solve_problem(Vars):-
    % declare variables
    % constrains
    labeling([], Vars).

diagonal_different_4([R1, R2, R3, R4]):-
    abs(R1-R2) #\= 1,
    abs(R1-R3) #\= 2,
    abs(R1-R4) #\= 3,
    abs(R2-R3) #\= 1,
    abs(R2-R4) #\= 2,
    abs(R3-R4) #\= 1.

solve_4queens(Sol):-
    domain(Sol,1,4),
    all_different(Sol),
    diagonal_different_4(Sol),
    labeling([], Sol).

dd(_, [], _):-!.
dd(A, [H|T], N):-
    abs(A-H) #\= N,
    N2 is N+1,
    dd(A, T, N2).

diagonal_different([]):-!.
diagonal_different([H|T]):-
    dd(H,T,1),
    diagonal_different(T).

solve_nqueens(Sol, N):-
    length(Sol, N),
    domain(Sol,1,N),
    all_different(Sol),
    diagonal_different(Sol),
    labeling([], Sol).

% knapsack
solve_knapsack(Capacity, Weights, Prices, Sol, P):-
    % Declarations
    length(Weights, N), length(Prices, N), length(Sol, N),
    domain(Sol, 0, Capacity),
    domain([W], 0, Capacity),
    domain([P], 0, sup),
    append(Sol, [P, W], Vars),

    % Conditions
    scalar_product(Weights, Sol, #=<, W),
    scalar_product(Prices, Sol, #=, P),

    % Solution
    labeling([maximize(P)], Vars).

%house move
solve_move(Time, People, TimeNeeded, PeopleNeeded, ScheduleTime, SchedulePeople):-
    % Declarations
    length(TimeNeeded, N), length(PeopleNeeded, N), length(ScheduleTime, N), length(SchedulePeople, N),
    domain(ScheduleTime, 0, Time),
    domain(SchedulePeople, 0, People),

    % Conditions
    ensure_people(People, TimeNeeded, PeopleNeeded, ScheduleTime, SchedulePeople),

    % Solution
    append(SchedulePeople, ScheduleTime, Vars),
    labeling([minimize(T)], Vars).
