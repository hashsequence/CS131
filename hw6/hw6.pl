/*
Name: Avery Wong
UID: 904582269
*/

duplist([],[]).
/*duplist([H],[H,H]).
duplist([H,H],[H]).*/
duplist([H1|T1],[H1,H1|T2]):-duplist(T1,T2).
duplist([H1,H1|T1],[H1|T2]):-duplist(T2,T1).

subseq([],_). /*case 1: empty set is subseq of all sequences*/
subseq([H1|T1],[H1|T2]):-subseq(T1,T2). /*case 2: head +  subsequences in T2*/
subseq([H1|T1],[_|T2]):-subseq([H1|T1],T2).  /*case3: otherwise there exist a subsequence in the rest of the list*/

/*
pickup(X,stack1).
pickup(X,stack2).
pickup(X,stack3).
putdown(X,stack1).
putdown(X,stack2).
putdown(X,stack3).
*/

/*
action(world([H|T],L2,F,none),pickup(H,stack1),putdown(H,stack2),world(T,[H|L2],F,none)).
action(world([H|T],F,L3,none),pickup(H,stack1),putdown(H,stack3),world(T,F,[H|L3],none)).
action(world(F,[H|T],L3,none),pickup(H,stack2),putdown(H,stack3),world(F,T,[H|L3],none)).
action(world(L1,[H|T],F,none),pickup(H,stack2),putdown(H,stack1),world([H|L1],T,F,none)).
action(world(L1,F,[H|T],none),pickup(H,stack3),putdown(H,stack1),world([H|L1],F,T,none)).
action(world(F,L2,[H|T],none),pickup(H,stack3),putdown(H,stack2),world(F,[H|L2],T,none)).

blocksworld(world(A,B,C,none), [], world(A,B,C,none)).
blocksworld(world(A1,A2,A3,none),[M1,M2],R):-action(world(A1,A2,A3,none),M1,M2,R1),blocksworld(R1,[],R).
blocksworld(world(A1,A2,A3,none),[M1,M2|T],R):-action(world(A1,A2,A3,none),M1,M2,R1),blocksworld(R1,T,R).
*/


action(world([H|F1],F2,F3,none),pickup(H,stack1),world(F1,F2,F3,H)) :- H\=none.
action(world(F1,[H|F2],F3,none),pickup(H,stack2),world(F1,F2,F3,H)) :- H\=none.
action(world(F1,F2,[H|F3],none),pickup(H,stack3),world(F1,F2,F3,H)) :- H\=none.
action(world(F1,F2,F3,H),putdown(H,stack1),world([H|F1],F2,F3,none)) :- H\=none.
action(world(F1,F2,F3,H),putdown(H,stack2),world(F1,[H|F2],F3,none)) :- H\=none.
action(world(F1,F2,F3,H),putdown(H,stack3),world(F1,F2,[H|F3],none)) :- H\=none.

blocksworld(A, [], A).
/*blocksworld(world(A1,A2,A3,X),[M],R):-action(world(A1,A2,A3,X),M,R1),blocksworld(R1,[],R).*/
blocksworld(world(A1,A2,A3,X),[M|T],R):-action(world(A1,A2,A3,X),M,R1),blocksworld(R1,T,R).

/*
helper functions for verbalarithmetic
basically, will enumerate all possible numbers
through brute force
*/
setnum(0).
setnum(1).
setnum(2).
setnum(3).
setnum(4).
setnum(5).
setnum(6).
setnum(7).
setnum(8).
setnum(9).

setnum1(1).
setnum1(2).
setnum1(3).
setnum1(4).
setnum1(5).
setnum1(6).
setnum1(7).
setnum1(8).
setnum1(9).


app([],L,L). 
app([H|T],L2,[H|L3])  :-  app(T,L2,L3).

rev([],[]).
rev([H|T],R) :- rev(T,R1),app(R1,[H],R).

enumtail([]).
enumtail([H|T]):-setnum(H),enumtail(T).

/*
helper functions that will sum them and make sures word1 + word2 = word3
*/

sum([],[],[],0).

sum([],[],[H1],1):-
H1 =:= 1.


sum([H1|T1],[H2|T2], [H3|T3], Carry):-
Carry =:= 1,
Temp1 is (H1 + H2 + Carry),
Temp2 is (H1 + H2 + Carry) mod 10,
Temp1 > 9,
H3 =:= Temp2,
sum(T1,T2,T3,1).

sum([H1|T1],[H2|T2], [H3|T3], Carry):-
Carry =:= 1,
Temp1 is (H1 + H2 + Carry),
Temp1 =< 9,
H3 =:= Temp1,
sum(T1,T2,T3,0).

sum([H1|T1],[H2|T2], [H3|T3], Carry):-
Carry =:= 0,
Temp1 is (H1 + H2),
Temp2 is (H1 + H2) mod 10,
Temp1 > 9,
H3 =:= Temp2,
sum(T1,T2,T3,1).

sum([H1|T1],[H2|T2], [H3|T3], Carry):-
Carry =:= 0,
Temp1 is (H1 + H2),
Temp1 =< 9,
H3 =:= Temp1,
sum(T1,T2,T3,0).



sum([H1|T1],[], [H3|T3], Carry):-
Carry =:= 0,
Temp1 is (H1),
Temp1 =< 9,
H3 =:= Temp1,
sum(T1,[],T3,0).

sum([H1|T1],[], [H3|T3], Carry):-
Carry =:= 1,
Temp1 is (H1 + Carry),
Temp1 =< 9,
H3 =:= Temp1,
sum(T1,[],T3,0).

sum([H1|T1],[], [H3|T3], Carry):-
Carry =:= 1,
Temp1 is (H1 + Carry),
Temp2 is (H1 + Carry) mod 10,
Temp1 > 9,
H3 =:= Temp2,
sum(T1,[],T3,1).




sum([],[H2|T2], [H3|T3], Carry):-
Carry =:= 0,
Temp1 is (H2),
Temp1 =< 9,
H3 =:= Temp1,
sum([],T2,T3,0).

sum([],[H2|T2], [H3|T3], Carry):-
Carry =:= 1,
Temp1 is (H2),
Temp1 =<  9,
H3 =:= Temp1,
sum([],T2,T3,0).

sum([],[H2|T2], [H3|T3], Carry):-
Carry =:= 1,
Temp1 is (H2 + Carry),
Temp2 is (H2 + carry) mod 10,
Temp1 >  9,
H3 =:= Temp2,
sum([],T2,T3,1).





verbalarithmetic(L,[H1|T1],[H2|T2],[H3|T3]):-
setnum1(H1), 
enumtail(T1),
setnum1(H2), 
enumtail(T2),
setnum1(H3), 
enumtail(T3),
fd_all_different(L),
rev([H1|T1],W),
rev([H2|T2],Y),
rev([H3|T3],Z),
sum(W,Y,Z,0).
