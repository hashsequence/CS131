list_member(X,[X|_]).
list_member(X,[_|T):-member(X,T).

/*merge(L1,L2,L) L is a merged sorted list of 2 sorted list L1 L2 */

merge(L,[],L).
merge([],L,L).
merge([H1|T1],[H2|T2],[H1|R]):-H1<=H2,merge(T1,[H2|T2],R).
merge([H1|T1],[H2|T2],[H2|R]):-H1>H2,merge([H1|T1],T2,R).
/*splits (L,L1,L2) splits l into L1 and l2 where every odd index is in L1 and rest in L2*/
split([],[],[])
split([X],[X],[])
/*split([H|T],[H|R1],R2):-split(T,R2,R1)*/
split([X,Y|L],[X|T1],[Y|T2]):-split(L,T1,T2).

/*mergsort*/
merge_sort([],[]).
merge_sort([H],[]).
merge_sort(L1,L2):split(L1,R1,R2),merge(R1,R2,L2).
