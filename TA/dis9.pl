/*list_length(L,N)*/
list_length([],0).
list_length([_|T],N):-list_length(T,N1), N is N1 + 1.

sumList([],0).
sumList([H|T],S):-sumList(T,S1),S is S1+H.

append1([],L,L).
append1([H|T],L2,[H|T3]):-append1(T,L2,T3).

list_prefix(P,L):-append1(P,_,L).

list_suffix(S,L):-append1(_,S,L).

shift_left([_|T],T).

shift_right(L,L1):-append(L1,[_],L).

rotate_left([H|T],L1):-append(T,H,L1).

rotate_right(L,[H|T]):-append(T,[H],L).

unzip([],[],[]).
unzip([(X,Y)|T1],[X|T2],[Y|T3]):-unzip(T1,T2,T3).
