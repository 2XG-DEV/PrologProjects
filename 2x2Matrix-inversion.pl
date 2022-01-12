invert([[A,B],[C,D]], [[IA,IB],[IC,ID]]) :-
    Det is A*D-B*C,
    Det \= 0,
    IDet is 1/Det,
    IA is IDet*D, IB is IDet*(-B),
    IC is IDet*(-C), ID is IDet*A.