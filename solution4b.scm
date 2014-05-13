
quicksort([], []).
quicksort([H | T], S) :-
   partition(T, H, Less, Same, Greater),
   quicksort(Less, SLess),
   quicksort(Greater, SGreater),
   append(SLess, [H | Same], SGreater, S).

append([], L, L).
append([H | T], L, [H | A]) :- append(T, L, A).

append(L1, L2, L3, A) :- append(L2, L3, L23), append(L1, L23, A).



%1 parition, takes a list and a pivot and breaks it into a lowerlist, samelist and upperlist.
partition([],P,A,B,C).
partition([H1|T1],P,[H1],B1,C1):- H1<P ,partition(T1,P,[H1],B2,C2).
partition([H1|T1],P,A1,[H1],C1):- H1=P ,partition(T1,P,A2,[H1],C2).
partition([H1|T1],P,A1,B1,[H1]):- H1>P ,partition(T1,P,A2,B2,[H1]).


%2 merge two lists together
merge(O1,[],O1).
merge([],O2,O2).
merge([H1|T1],[H2|T2],[H1|T]):- H1<H2,merge(T1,[H2|T2],T).
merge([H1|T1],[H2|T2],[H1|T]):- H2<H1,merge(T2,[H1|T1],T).
merge([H1|T1],[H2|T2],[H1|T]):- H2=H1,merge(T2,T1,T).