:-use_module(library(clpfd)).
:-use_module(library(lists)).

extract_credits([], []).
extract_credits([[_, C, _]|T], [C|CS]):-
    extract_credits(T, CS).

extract_requireds([], []).
extract_requireds([[_, C, 0]|T], [C|CS]):-!,
    extract_requireds(T, CS).
extract_requireds([[_, _, _]|T], [0|CS]):-!,
    extract_requireds(T, CS).

extract_subrequireds([], []).
extract_subrequireds([[_, C, 1]|T], [C|CS]):-!,
    extract_subrequireds(T, CS).
extract_subrequireds([[_, _, _]|T], [0|CS]):-!,
    extract_subrequireds(T, CS).

extract_optionals([], []).
extract_optionals([[_, C, 2]|T], [C|CS]):-!,
    extract_optionals(T, CS).
extract_optionals([[_, _, _]|T], [0|CS]):-!,
    extract_optionals(T, CS).

create_single_time_constraint(_, _, [], _).
create_single_time_constraint(
    [Start, End],
    Pick,
    [[Start2, End2]|Times2],
    Pick2
):-!,
    (Pick #= 0 #\/ Pick2 #= 0) #<= (Start #>= Start2 #/\ Start #=< End2),
    (Pick #= 0 #\/ Pick2 #= 0) #<= (Start2 #>= Start #/\ Start2 #=< End),
    create_single_time_constraint([Start, End], Pick, Times2, Pick2).

create_time_constraint([], _, _, _).
create_time_constraint([Time|Times], Pick, Times2, Pick2):-!,
    create_single_time_constraint(Time, Pick, Times2, Pick2),
    create_time_constraint(Times, Pick, Times2, Pick2).

create_time_constraints(_, _, [], []).
create_time_constraints(
    Times,
    Pick,
    [[Times2, _, _]|Lectures],
    [Pick2|Picks]
):-!,
    create_time_constraint(Times, Pick, Times2, Pick2),
    create_time_constraints(Times, Pick, Lectures, Picks).   

create_constraints([], []).
create_constraints([[Times, _, _]|Lectures], [Pick|Picks]):-!,
    create_time_constraints(Times, Pick, Lectures, Picks),
    create_constraints(Lectures, Picks).

create_lunch_lectures(
    Current,
    End,
    Duration,
    [[[[Current, E]], 0, 0]|Lectures]
):-
    Current2 is Current + 1,
    E is Current + Duration,
    E =< End,!,
    create_lunch_lectures(Current2, End, Duration, Lectures).
create_lunch_lectures(Current, End, Duration, []):-!,
    E is Current + Duration,
    E > End.

create_all_lunch_lectures(0, _, _, [], []).
create_all_lunch_lectures(1, [], _, [], []).
create_all_lunch_lectures(
    1,
    [[Start, End]|Times],
    Duration,
    [Lectures|LecturesTail],
    [Picks|PicksTail]
):-!,
    create_lunch_lectures(Start, End, Duration, Lectures),
    length(Lectures, N), length(Picks, N),
    domain(Picks, 0, 1),
    sum(Picks, #=, 1),
    create_all_lunch_lectures(1, Times, Duration, LecturesTail, PicksTail).

% - Lectures = [[[[start, stop], ...], credits, priority], ...]
%     Priority = 0 .. required, 1 .. subrequired, 2 .. optional
% - Options = [MinCredits, MinRequiredCredits, MinSubrequiredCredits, LunchBreak, LunchDuration, ReqRatio, SubreqRatio, OptionalRatio]
% + Picks
% + CreditSum
schedule(
    Lectures,
    [
        MinCredits,
        MinReqs,
        MinSubreqs,
        LunchBreak,
        LunchDuration,
        ReqRatio,
        SubreqRatio,
        OptionalRatio
    ],
    Picks,
    CreditSum
):-
    length(Lectures, N), length(Picks, N), length(Credits, N),
    domain(Picks, 0, 1), domain(Credits, 1, 99),

    extract_requireds(Lectures, Reqs),
    extract_subrequireds(Lectures, Subreqs),
    extract_optionals(Lectures, Optionals),

    scalar_product(Reqs, Picks, #=, ReqsSum),
    scalar_product(Subreqs, Picks, #=, SubreqsSum),
    scalar_product(Optionals, Picks, #=, OptionalsSum),
    CreditSum #= ReqsSum + SubreqsSum + OptionalsSum,

    Objective #= ReqRatio * ReqsSum + 
                 SubreqRatio * SubreqsSum + 
                 OptionalRatio * OptionalsSum,

    CreditSum #>= MinCredits,
    ReqsSum #>= MinReqs,
    SubreqsSum #>= MinSubreqs,

    format('Creating lunch variables.\n', []),

    create_all_lunch_lectures(LunchBreak, [
        [66, 90], [210, 234], [354, 378],
        [498, 522], [642, 666], [786, 810], [930, 954]
    ], LunchDuration, LunchLectures, LunchPicks),

    append(LunchLectures, LunchLs),
    append(LunchPicks, LunchPs),
    append(Lectures, LunchLs, Lectures2),
    append(Picks, LunchPs, Picks2),

    format('Variables ready, generating constraints.\n', []),

    create_constraints(Lectures2, Picks2),

    format('Constraints generated, looking for optimum.\n', []),

    labeling([maximize(Objective)], Picks2),

    format('Optimum found.\n', []).

% Monday:      0 -  143
% Tuesday:   144 -  287
% Wednesday: 288 -  431
% Thursday:  432 -  575
% Friday:    576 -  719
% Saturday:  720 -  863
% Sunday:    864 - 1007

% Test:
% :-
%     statistics(runtime, [Start|_]),
%     schedule(
%         [
%             [[[630, 639]], 3, 1],                        % NAIL013
%             [[[198, 207]], 3, 1],                        % NAIL052
%             [[[248, 257]], 3, 1],                        % NAIL061
%             [[[238, 247], [640, 649]], 3, 1],            % NAIL065
%             [[[218, 227], [ 54,  63]], 5, 1],            % NAIL068 v1
%             [[[218, 227], [ 64,  73]], 5, 1],            % NAIL068 v2
%             [[[372, 381]], 6, 1],                        % NAIL074
%             [[[ 84,  93]], 3, 1],                        % NAIL077
%             [[[516, 525], [526, 535]], 6, 1],            % NAIL086
%             [[[372, 381], [506, 515]], 6, 1],            % NAIL094
%             [[[342, 351], [352, 361]], 6, 1],            % NAIL101
%             [[[496, 505], [382, 391]], 6, 1],            % NAIL106 v1
%             [[[496, 505], [536, 545]], 6, 1],            % NAIL106 v2
%             [[[486, 490], [491, 495]], 6, 1],            % NAIL108
%             [[[ 74,  83], [228, 237], [352, 361]], 9, 1],  % NDBI023
%             [[[630, 639], [640, 649]], 5, 1],            % NMAI061
%             [[[342, 351], [352, 361]], 6, 1],            % NPFL068
%             [[[218, 222], [223, 227]], 3, 1],            % NPFL097
%             [[[634, 641]], 9, 1],                        % NPRG023
%             [[[ 74,  83], [218, 227]], 6, 1],            % NSWE001
%             [[[ 54,  72]], 3, 2],                        % NSWI054
%             [[[504, 517]], 1, 0],                        % Testing lecture
%             [[[490, 497]], 15, 2]                        % Testing lecture
%         ], [30, 0, 0, 1, 4, 20, 15, 8], P, C
%     ),
%     statistics(runtime, [Stop|_]),
%     R is (Stop - Start) / 1000,
%     format('\n  Picks: ~w\n',  [P]),
%     format('Credits: ~w\n',    [C]),
%     format('   Took: ~ws\n\n', [R]).

% Test for lunch
% :-
%     schedule(
%         [
%             [[[0,9]],1,0],
%             [[[10,19]],1,0],
%             [[[20,29]],1,0],
%             [[[30,39]],1,0],
%             [[[40,49]],1,0],
%             [[[50,59]],1,0],
%             [[[60,69]],1,0],
%             [[[70,79]],1,0],
%             [[[80,89]],1,0],
%             [[[90,99]],1,0],
%             [[[100,109]],1,0],
%             [[[110,119]],1,0],
%             [[[120,129]],1,0],
%             [[[130,139]],1,0]
%         ], [0,0,0,1,4,1,1,1], P, C
%     ).