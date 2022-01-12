:- module(matrix_multiply,
    [matrix_multiply/3
    ,dot_product/3
    ]).
:- use_module(library(clpfd), [transpose/2]).

%%  matrix_multiply(+X,+Y,-M) is det.
%
%   X(N*P),Y(P*M),M(N*M)
%
matrix_multiply(X,Y,M) :-
    transpose(Y,T),
    maplist(row_multiply(T),X,M).

row_multiply(T,X,M) :-
    maplist(dot_product(X),T,M).

dot_product([X|Xs],[T|Ts],M) :-
    foldl(mul,Xs,Ts,X*T,M).
mul(X,T,M,M+X*T).