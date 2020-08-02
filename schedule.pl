:-use_module(library(clpfd)).
:-use_module(library(lists)).


domain([], _, _).
domain([H|T], I, A):-
    H #>= I,
    A #>= H,
    domain(T, I, A).

% Found [online](https://stackoverflow.com/questions/5628451/prolog-dot-product-with-variables-constraint-satisfaction)
my_scalar_product([V|Vs], [C|Cs], Op, Value) :-
    construct_constraint(Vs, Cs, (V * C), Constr),
    Constraint =.. [Op, Constr, Value],
    Constraint.

construct_constraint([], [], Acc, Acc).
construct_constraint([V|Vs], [F|Fs], Acc, Res) :-
    construct_constraint(Vs, Fs, '+'(Acc, (V * F)), Res).


% Ensures same length of multiple lists
have_same_length([], 0).
have_same_length([H|[]], N):-
    !, length(H, N).
have_same_length([H|T], N):-
    length(H, N),
    have_same_length(T, N).

% Minutes in hour ... 60
% Minutes in day  ... 1440
% Minutes in week ... 10080

% Schedule
% +credits    ... number of credits of each subject
% +priorities ... priority of each subject [0 = optional; 1 = obligatorily optional; 2 = obligatory]
% +starts     ... start times of each subject in minutes from start of the week
% +durations  ... durations of each subject in minutes
% +minimum    ... minimum acceptable credits
% +maximum    ... maximum acceptable credits
% +lunch_break... [0 = no lunch considered, 1 = lunch break is a must]
% -picks      ... picked subjects [0 = not picked; 1 = picked]
% -credit_sum ... number of obtained credits
schedule(Credits, Priorities, Starts, Durations, Minimum, Maximum, LunchBreak, Picks, CreditSum):-
    % Credits, Priorities, Starts, Durations and Picks must have same length
    have_same_length([Credits, Priorities, Starts, Durations, Picks], _),
    
    % Table is used for placement of constraints on time slots
    % (1 slot = 1 minute) [1 = taken, 0 = free]
    have_same_length([Table], 10080),
    
    % When time breaks start
    LunchStarts = [660,2100,3540,4980,6420,7860,9300],
    
    % When time breaks end
    LunchEnds   = [900,2340,3780,5220,6660,8100,9540],

    % Just to assert valid input values
    domain(Credits, 0, 60),
    domain(Priorities, 0, 2),
    domain(Starts, 0, 10079),
    domain(Durations, 1, 1440),
    domain(Picks, 0, 1),
    domain(Table, 0, 1),

    % We might have additional requirements on gotten credits (60 is too much
    % for 1 semester, with 15 we wouldn't have passed, ...)
    CreditSum #> Minimum,
    CreditSum #< Maximum,

    % Ensure that lessons do not overlap, prefer required lessons
    create_time_constraints(Starts, Durations, Priorities, Picks, Table),

    % Ensure that there is time for lunch if needed
    create_lunch_constraints(LunchBreak, LunchStarts, LunchEnds, Table),

    % Compute the credit sum
    my_scalar_product(Picks, Credits, #=, CreditSum),

    % Solve the CSP
    labeling([max(CreditSum)], Picks).

% Creates lunch constraints (ensuring 60 minutes free for lunch during lunchtime)
% 0 = no lunch time
create_lunch_constraints(0, _, _, _).

create_lunch_constraints(1, [], [], _). % Stop condition
create_lunch_constraints(1, [LSH|LST], [LEH|LET], Table):-
    (  LSH = 0
    -> X = 1
    ;  X = 0
    ),
    Y is LEH - 60, % 60 minutes for lunch
    % From LSH to LEH where Y is the last start time, 0 indicates current
    % starting time, X indicates whether we are in lunchtime zone of table,
    % acc, table and output table go next.

    ZL is Y - LSH + 1,
    length(Z, ZL),
    % Exactly one of Zs must be true
    domain(Z, 0, 1),
    sum(Z, #=, 1),

    create_lunch_constraint(LSH, LEH, Y, 0, X, Table, Z),!,

    % Continue for the rest of lunchtimes
    create_lunch_constraints(1, LST, LET, Table).

% Create a single lunchtime constraint.
create_lunch_constraint(_,E,Y,Y,_,[TH|TT],[ZH]):- % Stop condition
    lunch_constraint(ZH, Y, E, [TH|TT]).

% Lunchtime not started yet, continue in table
create_lunch_constraint(S,E,Y,I,0,[_|TT],Z):-
    I2 is I+1,
    (  I2 = S
    -> X = 1
    ;  X = 0
    ),!,
    create_lunch_constraint(S,E,Y,I2,X,TT,Z).

% Lunchtime started, create lunch constraint for now_is_lunch or now+1_is_lunch or ... 
create_lunch_constraint(S,E,Y,I,1,[TH|TT],[ZH|ZT]):-
    I2 is I+1,
    EI is I+60,
    lunch_constraint(ZH, I, EI, [TH|TT]),!, % Create lunch constraint
    create_lunch_constraint(S,E,Y,I2,1,TT,ZT).

% Create a single lunch constraint
lunch_constraint(_, E, E, _).
lunch_constraint(P, S, E, [TH|TT]):-
    S1 is S+1,
    P #==> TH #= 0,!, % If lunch is chosen, then time slot must be free
    lunch_constraint(P, S1, E, TT).

% Creates lecture constraints (that no two lectures can overlap)
create_time_constraints([], [], [], [], _). % Stop constraint

% + S  ... When does the lecture Start
% + D  ... How long is the lecture (Durations)
% + R  ... What is the lecture's pRiority
% - P  ... Boolean list identifying our Picks
% + T  ... Current bookings of time slots (Table)
% - T3 ... Final bookings of time slots (Table 3)
create_time_constraints([SH|ST], [DH|DT], [RH|RT], [PH|PT], T):-
    % Create time constraint
    create_time_constraint(SH, DH, RH, PH, ST, DT, RT, PT),!,
    % Ensure the constraint books the table time slot if needed
    create_table_constraint(PH, SH, DH, T),!,
    % Recurse
    create_time_constraints(ST, DT, RT, PT, T).

% Create a table constraint for single lecture
create_table_constraint(P, S, D, T):-
    E is S + D, % End time
    % Create table constraint
    table_constraint(P, S, E, T, 0, 0).

% Table constraint, go through time slots until you find our slots
table_constraint(P, S, E, [_|TT], I, 0):-
    I2 is I + 1,
    (  I2 < S
    -> X=0
    ;  X=1
    ),!,
    table_constraint(P, S, E, TT, I2, X).

% When right time slots found, book them if picked
table_constraint(P, _, E, [TH|_], E, 1):- % Stopping condition
    P #==> TH #= 1. % Book if picked

table_constraint(P, S, E, [TH|TT], I, 1):- 
    I2 is I + 1,
    P #==> TH #= 1, % Book if picked
    table_constraint(P, S, E, TT, I2, 1).

% Create time constraint ensuring no two lectures overlap
create_time_constraint(_, _, _, _, [], [], [], []).
create_time_constraint(S, D, R, P, [SH|ST], [DH|DT], [RH|RT], [PH|PT]):-
    % Our priority is higher (don't pick them) [we don't care about us]
               (PH #= 0) #<== ((S #>= SH #/\ S #=< SH+DH) #/\ R #> RH),
               (PH #= 0) #<== ((SH #>= S #/\ SH #=< S+D) #/\ R #> RH),
    % Our priority is the same (don't pick us) [we don't care about the other]
                (P #= 0) #<== ((S #>= SH #/\ S #=< SH+DH) #/\ R #< RH),
                (P #= 0) #<== ((SH #>= S #/\ SH #=< S+D) #/\ R #< RH),
    % Our priorities are the same (do not pick both of us) [both of us might not get picked]
    (PH #= 0 #\/ P #= 0) #<== ((S #>= SH #/\ S #=< SH+DH) #/\ R #= RH),
    (PH #= 0 #\/ P #= 0) #<== ((SH #>= S #/\ SH #=< S+D) #/\ R #= RH),
    create_time_constraint(S, D, R, P, ST, DT, RT, PT).

% Example:
% schedule([4,5],[1,0],[0,5],[10,10],0,100,0,P,C).
% schedule([credits], [priorities], [starts], [durations], min, max, lunch, P, C).

% When time breaks start
% LunchStarts = [660,2100,3540,4980,6420,7860,9300]

% When time breaks end
% LunchEnds   = [900,2340,3780,5220,6660,8100,9540]

% Monday    --    0 -  1439
% Tuesday   -- 1440 -  2879
% Wednesday -- 2880 -  4319
% Thursday  -- 4320 -  5759
% Friday    -- 5760 -  7199
% Saturday  -- 7200 -  8639
% Sunday    -- 8640 - 10079

% NAIL013   --  540 -   630 -- 1 -- 3
% NAIL052   -- 1980 -  2070 -- 1 -- 3
% NAIL061   -- 2480 -  2570 -- 1 -- 3
% NAIL065   -- 2380 -  2470 -- 1 -- 5
% NAIL068   -- 2180 -  2270 -- 1 -- 6
% NAIL074   -- 3320 -  3410 -- 1 -- 3
% NAIL077   --  840 -   930 -- 1 -- 3
% NAIL086   -- 5160 -  5350 -- 1 -- 6
% NAIL088   -- not tought
% NAIL094   -- 3720 -  3810 -- 1 -- 6
% NAIL101   -- 3420 -  3610 -- 1 -- 6
% NAIL105   -- not tought
% NAIL106   -- 4960 -  5050 -- 1 -- 6
% NAIL108   -- 4860 -  4950 -- 1 -- 3
% NDBI023   -- 3520 -  3610 -- 1 -- 9
% NMAI061   -- 6300 -  6490 -- 1 -- 5
% NPFL068   -- 3420 -  3610 -- 1 -- 6
% NPFL097   -- 2180 -  2270 -- 1 -- 3
% NPRG023   -- 6400 -  6490 -- 1 -- 9
% NPRG027   -- not tought
% NSWE001   --  740 -   830 -- 1 -- 6
% NSWI054   --  540 -   720 -- 0 -- 3
% TEST      -- 5060 -  5170 -- 2 -- 1

% schedule([3,3,3,5,6,3,3,6,6,6,6,3,9,5,6,3,9,6,3,1],
%          [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,2],
%          [540, 1980, 2480, 2380, 2180, 3320, 840, 5160, 3720, 3420, 4960,
%           4860, 3520, 6300, 3420, 2180, 6400, 740, 540, 5060],
%          [90, 90, 90, 90, 90, 90, 90, 190, 90, 190, 90,
%           90, 90, 190, 190, 90, 190, 90, 190, 190],
%          20, 50, 1, P, C).
% P = [0, 0, 0, 1, 0, 1, 1, 0, 1|...],
% C = 49 