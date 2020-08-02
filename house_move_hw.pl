:-use_module(library(clpfd)).

% creates domains for schedules
make_schedule_domains([], [], _):-!.
make_schedule_domains([SH | ST], [TH | TT], Time):-
    X is Time-TH,
    domain([SH], 0, X),
    make_schedule_domains(ST, TT, Time).

% Creates allocation subarrays
create_sub_arrays([], _, _):-!.
create_sub_arrays([H|T], N, People):-
	length(H, N),
	domain(H, 0, People),
	create_sub_arrays(T, N, People).

% Fills allocations for an item. Allocation in time T is 0 if the item is not moving or Need if the item is moving.
fill_allocations([], _, _, _, _):-!.
fill_allocations([AH|AT], Idx, Start, End, Need):-
	((Idx #< Start #\/ Idx #>= End) #=> AH #= 0),
	((Idx #>= Start #/\ Idx #< End) #=> AH #= Need),
	Idx2 is Idx+1,
	fill_allocations(AT, Idx2, Start, End, Need).

% Creates conditions for allocations for all items
assert_people_at_times([], [], [], []):-!.
assert_people_at_times([Start|ST], [A|AT], [Takes|TT], [PH|PT]):-
    End #= Start + Takes,
	fill_allocations(A, 0, Start, End, PH),
	assert_people_at_times(ST, AT, TT, PT).

% Adds two lists together and returns the result. Adding is done via conditions.
add_lists([], [], []):-!.
add_lists([H1|T1], [H2|T2], [RH|RT]):-
	RH #= H1 + H2,
	add_lists(T1, T2, RT).
	
% Sums 2D array by columns using conditions.
assert_sum_each([A|[]], A):-!.
assert_sum_each([AH1, AH2 | AT], Res2):-
	add_lists(AH1, AH2, Res),
	assert_sum_each([Res|AT], Res2).
	
% Asserts that all allocations are consistent.
assert_sum(Allocations, People, Time, Res):-
	length(Res, Time),
	assert_sum_each(Allocations, Res),
	domain(Res, 0, People).
	
% Solves the moving problem.
solve_move(Time, People, TimeNeeded, PeopleNeeded, Schedule):-
    % Declarations
    length(TimeNeeded, N), length(PeopleNeeded, N), length(Schedule, N),
	length(Order, N),

    % Make domains
	make_schedule_domains(Schedule, TimeNeeded, Time),
	NN is N-1,
	domain(Order, 0, NN),
	
    % Conditions
    % Sum of people at a time must be lower or equal to people
    assert_people_at_times(Schedule, Allocations, TimeNeeded, PeopleNeeded),
	assert_sum(Allocations, People, Time, Res),

	append(Res, Schedule, Vars),
	
    % Solution
    labeling([], Vars).
