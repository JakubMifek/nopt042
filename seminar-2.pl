memberX(X, [X|_]).
memberX(X, [_|T]):-member(X,T).

% deleteX(L, X, O):-delete x from l, return in o
deleteX([], _, []).
deleteX([X|T], X, T).
deleteX([Y|T], X, [Y|O]):-deleteX(T, X, O),X\=Y.

% insert X to L
insertX(L,X,[X|L]).

% insert X to the end of L
insertXEnd([],X,[X]).
%insertXEnd([Y|[]],X,[Y,X]):-!.
insertXEnd([H|T],X,[H|T2]):-insertXEnd(T,X,T2).

% concat
concatL([], L2, L2).
concatL([H|T], L2, [H|L3]):-concatL(T, L2, L3).

% reverseL
reverseL2([], A, A).
reverseL2([H|T], A, O):-reverseL2(T, [H|A], O),!.

reverseL([], []).
reverseL(X, O):-reverseL2(X,[],O).
