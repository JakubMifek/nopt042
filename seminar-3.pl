:- use_module(library(clpfd)).

arithmetics_gt(A, B):-member(A, [1,2,3,4,5,6,7,8,9,10]), B is 10-A.

% SEND+MORE=MONEY
% HW
smm(Sol):-
  Sol=[S,E,N,D,M,O,R,Y],
  Carry=[C1, C2, C3],
  domain([E,N,D,O,R,Y], 0, 9),
  domain([S,M], 1, 9),
  domain(Carry, 0, 1),
  Y+10*C1 #= E+D,
  E+10*C2 #= R+N+C1,
  N+10*C3 #= E+O+C2,
  O+10*M #= S+M+C3,
  all_different(Sol),
  append(Sol, Carry, Vars),
  labeling([], Vars).


% DONALD+GERALD=ROBERT
dgr(Sol):-
  Sol=[D,O,N,A,L,G,E,R,B,T],
  Carry=[C1, C2, C3, C4],
  domain(Sol,0,9),
  domain([D,G,R],1,9),
  domain(Carry,0,1),
  T+10*C1 #= D+D,
  R+10*C2 #= L+L+C1,
  E+10*C3 #= A+A+C2,
  B+10*C4 #= R+N+C3,
  O+10*C5 #= E+O+C4,
  R       #= G+D+C5,
  all_different(Sol),
  append(Sol, Carry, Vars),
  labeling([], Vars).