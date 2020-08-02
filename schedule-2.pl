:-use_module(library(clpfd)).

extract_credits([], []).
extract_credits([[_, C, _]|T], [C|CS]):-
    extract_credits(T, CS).

extract_requireds([], []).
extract_requireds([[_, C, 0]|T], [C|CS]):-
    !,
    extract_requireds(T, CS).
extract_requireds([[_, _, _]|T], [0|CS]):-
    !,
    extract_requireds(T, CS).

extract_subrequireds([], []).
extract_subrequireds([[_, C, 1]|T], [C|CS]):-
    !,
    extract_subrequireds(T, CS).
extract_subrequireds([[_, _, _]|T], [0|CS]):-
    !,
    extract_subrequireds(T, CS).

extract_optionals([], []).
extract_optionals([[_, C, 2]|T], [C|CS]):-
    !,
    extract_optionals(T, CS).
extract_optionals([[_, _, _]|T], [0|CS]):-
    !,
    extract_optionals(T, CS).

% Lectures = [[[[start, stop], ...], credits, priority], ...]
% Priority = 0 .. required, 1 .. subrequired, 2 .. optional
% LunchBreak = 0 .. no, 1 .. yes
schedule(Lectures, [MinCredits, MinReqs, MinSubreqs], LunchBreak, Picks, CreditSum):-
    length(Lectures, N), length(Picks, N), length(Credits, N),
    domain(Picks, 0, 1), domain(Credits, 1, 99),

    extract_requireds(Lectures, Reqs),
    extract_subrequireds(Lectures, Subreqs),
    extract_optionals(Lectures, Optionals),

    scalar_product(Reqs, Picks, #=, ReqsSum),
    scalar_product(Subreqs, Picks, #=, SubreqsSum),
    scalar_product(Optionals, Picks, #=, OptionalsSum),
    CreditSum #= ReqsSum + SubreqsSum + OptionalsSum,

    Objective #= 10000 * (10000 * (10000 * ReqsSum) + SubreqsSum) + OptionalsSum,

    CreditSum #>= MinCredits,
    ReqsSum #>= MinReqs,
    SubreqsSum #>= MinSubreqs,

    

    labeling([maximize(Objective)], Picks).