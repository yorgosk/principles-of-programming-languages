/*
Author: Georgios Kamaras, 1115 2014 00058
Date: 7/12/2016
*/

%---------------------------------------------------------------------------------------------------------------------

%problem 1 - pokemon go

/* take a (pokemon, candies needed to evolve, candies available) tuple and return a (pokemon, evolutions) tuple */
evolve((X,Y,Z),(X,D)) :-
  A is div(Z,Y),
  B is A*2,
  C is Z+B,
  D is div(C,Y).

/* take a list of (pokemon, candies needed to evolve, candies available) tuples and formulate a list of (pokemon, evolutions) tuples */
evolution([E],[K]) :- evolve(E,K).
evolution([H|T],[U|W]) :-
  evolution(T,W),
  evolve(H,U).

/* from two pairs of (pokemon, evolutions) find the one with maximum evolutions */
maxOfTwo((A,B),(_,D),(A,B)) :- B >= D.
maxOfTwo((_,B),(C,D),(C,D)) :- B < D.

/* to help me find the pair of (pokemon, evolutions) with maximum evolutions */
max([E],E).
max([H|T],(S,C)) :- max(T,I), maxOfTwo(H,I,(S,C)).

/* a rule to assist me when summing all evolutions*/
sum((_,Y),W,Z) :- Z is Y+W.

/* sum all evolutions */
sumList([(_,Y)],N) :- N is Y.
sumList([H|T],N) :- sumList(T,I), sum(H,I,N).

/* find the pair of (pokemon, evolutions) with maximum evolutions and sum all evolutions */
findNS(W,N,S) :- max(W,(S,_)), sumList(W,N).

/* first evolve the pokemons and then find N and S */
pokemon(L,N,S) :- evolution(L,W), findNS(W,N,S).

%---------------------------------------------------------------------------------------------------------------------

%problem 2 - plane food problem

/* calculate additional serving time based on the seat's letter */
handover(f,T,Time) :- Time is T+1.
handover(e,T,Time) :- Time is T+2.
handover(d,T,Time) :- Time is T+3.
handover(a,T,Time) :- Time is T+4.
handover(b,T,Time) :- Time is T+5.
handover(c,T,Time) :- Time is T+6.

/* my rule from moving the flight assistants along the corridor */
move(R,_,R,S,T,Time) :- handover(S,T,Time).
move(_,R,R,S,T,Time) :- handover(S,T,Time).
move(A,B,R,S,T,Time) :-
  1 =:= mod(A,2),
  C is A+1,
  D is B+1,
  T1 is T+7,  % 6 seats and 1 row later
  move(C,D,R,S,T1,Time).
move(A,B,R,S,T,Time) :-
  0 =:= mod(A,2),
  C is A+3,
  D is B+3,
  T1 is T+9,  % 6 seats and 3 rows later
  move(C,D,R,S,T1,Time).

/* move flight assistants along the corridor and calculate time to serve the specified seat */
mylunch(R,S,T) :- move(1,3,R,S,0,Time), T is Time.

%---------------------------------------------------------------------------------------------------------------------

%problem 3 - palindromic list

%Reverse list using an accumulator
accRev([],A,A).
accRev([H|T],A,R) :- accRev(T,[H|A],R).
reverse(L,R) :- accRev(L,[],R).

rec(L,L,L). %base case
rec([H|TL],[H|TR],[H|L1]) :-
  rec(TL,TR,L1).
rec([HL,EL|TL],[HR|TR],L1) :-
  HL<HR,
  A is HL+EL,
  rec([A|TL],[HR|TR],L1).
rec([HL|TL],[HR,ER|TR],L1) :-
  HR<HL,
  A is HR+ER,
  rec([HL|TL],[A|TR],L1).

%based on the observation that for every modification we have one less element
palindromic(L,N) :-
  reverse(L,R),
  rec(L,R,L1),
  length(L,LLEN),
  length(L1,L1LEN),
  N is LLEN-L1LEN.

%---------------------------------------------------------------------------------------------------------------------

%problem 4 - mymatrix

%construct row
%R: row, C: column, S: sum of row so far, M:middle element, P: previous element

%last column, any row for N%2==0
row(_,C,S,M,N,_,_,[E]) :-   %write("In last\n"),
  0 is mod(N,2),
  C is N,                 %write("In 1\n"),
  E is (N*M)-S.

%last column, any row for N%2==1
row(_,C,S,M,N,_,_,[E]) :-   %write("In last\n"),
  1 is mod(N,2),
  C is N,                 %write("In 1\n"),
  E is (N*M)-S.

%last row, first element for N%2==0
row(R,C,S,M,N,X,_,[E|L]) :-
  0 is mod(N,2),
  R is N,
  C is 1,
  E is N*((N/2)*2*N+1)-X,
  C1 is C+1,
  S1 is S+E,
  row(R,C1,S1,M,N,X,E,L).

%last row, first element for N%2==1
row(R,C,S,M,N,X,_,[E|L]) :-
  1 is mod(N,2),
  R is N,
  C is 1,
  E is N*((round(N/2)-1)*N+1)-X,
  C1 is C+1,
  S1 is S+E,
  row(R,C1,S1,M,N,X,E,L).

%any row, first element for N%2==0
row(R,C,S,M,N,X,_,[E|L]) :-   %write("In 0\n"),
  0 is mod(N,2),
  C is 1,
  E is ((R-1)*2*N)+C,     %write("In 2\n"),
  C1 is C+1,              %write("In 3\n"),
  S1 is S+E,              %write("In 4\n"),
  X is E,
  row(R,C1,S1,M,N,X,E,L).

%any row, first element for N%2==1
row(R,C,S,M,N,X,_,[E|L]) :-   %write("In 0\n"),
  1 is mod(N,2),
  C is 1,
  E is ((R-1)*N)+C,     %write("In 2\n"),
  C1 is C+1,              %write("In 3\n"),
  S1 is S+E,              %write("In 4\n"),
  X is E,
  row(R,C1,S1,M,N,X,E,L).

%any row, middle element for N%2==0
row(R,C,S,_,N,X,P,[E|L]) :-   %write("In 0\n"),
  0 is mod(N,2),
  C is N/2+1,
  E is P+1,               %write("In 2\n"),
  %M is E,
  C1 is C+1,              %write("In 3\n"),
  S1 is S+E,              %write("In 4\n"),
  row(R,C1,S1,E,N,X,E,L).

%any row, middle element for N%2==1
row(R,C,S,_,N,X,P,[E|L]) :-   %write("In 0\n"),
  1 is mod(N,2),
  C is round(N/2),
  E is P+1,               %write("In 2\n"),
  %M is E,
  C1 is C+1,              %write("In 3\n"),
  S1 is S+E,              %write("In 4\n"),
  row(R,C1,S1,E,N,X,E,L).

%any row, second element onwards for any N
row(R,C,S,M,N,X,P,[E|L]) :-   %write("In 0\n"),
  E is P+1,               %write("In 2\n"),
  C1 is C+1,              %write("In 3\n"),
  S1 is S+E,              %write("In 4\n"),
  row(R,C1,S1,M,N,X,E,L).

createrow(R,N,X,L) :- row(R,1,0,0,N,X,0,L).

creatematrix(N,R,X,[H]) :-
  N is R,
  createrow(R,N,X,H).  % I pass the sum
creatematrix(N,R,X,[H|T]) :-  % X: sum of rows' first elements
  createrow(R,N,FE,H),
  R1 is R+1,
  Y is X+FE,
  creatematrix(N,R1,Y,T).

mymatrix(N,M) :- creatematrix(N,1,0,M).

%---------------------------------------------------------------------------------------------------------------------

%problem 5 - myseat

/* if occupied 'o' seat other than the first one and we already have found some (possibly) optimal seat then return */
findseat([o],F,PM,M) :-
  F > 0,
  PM =\= -1,
  M is -1.

/* check if last seat is empty */
findseat(L,F,_,M) :-
  F is 0,
  last(L,e),
  length(L,LEN),
  M is LEN-1.

/* check if first seat is empty */
findseat([e|_],F,_,M) :-
  F is 0,
  M is F.

/* in case that only single seats are empty, like [o,e,o,e,o], we want to seat in the rightmost one */
findseat([e|T],F,_,M) :-
  M1 is F,
  F1 is F+1,
  findseat(T,F1,M1,M2),
  M is max(M1,M2).

/* when we get the first [o,e,e] sequence, then the rightmost empty 'e' seat is the optimal */
findseat([o,e,e|_],F,_,M) :- M is F+2.

/* in case of single occupied seat, we continue */
findseat([o|T],F,PM,M) :- F1 is F+1, findseat(T,F1,PM,M).

/* if more than three seats are passed */
findseat([H|T],F,PM,M) :-
  findseat(H,F,PM,M1),
  F1 is F+1,
  findseat(T,F1,M1,M2),
  M is max(M1,M2).

/* start from the first seat and move backwards to find the optimal seat. */
myseat(L,M) :- findseat(L,0,-1,M).

%---------------------------------------------------------------------------------------------------------------------
