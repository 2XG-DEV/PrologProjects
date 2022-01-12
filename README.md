# PrologProjects

## Easier Projects


1. Define a predicate to "flatten" a list by construction a list containing no lists as elements , but containing all of the atoms of the original list.
For Example, the following goal would succeed:

?- flatten([a,[b,c],[d,[],e]],[a,b,c,d,e]).

```
flatten2([], []) :- !.
flatten2([L|Ls], FlatL) :-
    !,
    flatten2(L, NewL),
    flatten2(Ls, NewLs),
    append(NewL, NewLs, FlatL).
flatten2(L, [L]).

```

Recursively reduces all lists of lists into either single item lists [x], or empty lists [] which it throws away. 
Then, it accumulates and appends them all into one list again out the output.

Append in practice:

append(?List1, ?List2, ?List1AndList2)

List1AndList2 is the concatenation of List1 and List2


```
?- append([a,b], [c], X).
X = [a,b,c].

?- append(X, [Last], [a,b,c]).
X = [a,b],
Last = c.

?- append([a,b], More, List).
List = [a,b|More].

```


Call trace for above example:

```
Call:flatten2([a, [b, c], [[d], [], e]], [a, b, c, d, e])
 Call:flatten2(a, _6148)
 Exit:flatten2(a, [a])
 Call:flatten2([[b, c], [[d], [], e]], _6156)
 Call:flatten2([b, c], _6158)
 Call:flatten2(b, _6160)
 Exit:flatten2(b, [b])
 Call:flatten2([c], _6168)
 Call:flatten2(c, _6170)
 Exit:flatten2(c, [c])
 Call:flatten2([], _6178)
 Exit:flatten2([], [])
 Call:lists:append([c], [], _6168)
 Exit:lists:append([c], [], [c])
 Exit:flatten2([c], [c])
 Call:lists:append([b], [c], _6158)
 Exit:lists:append([b], [c], [b, c])
 Exit:flatten2([b, c], [b, c])
 Call:flatten2([[[d], [], e]], _6192)
 Call:flatten2([[d], [], e], _6194)
 Call:flatten2([d], _6196)
 Call:flatten2(d, _6198)
 Exit:flatten2(d, [d])
 Call:flatten2([], _6206)
 Exit:flatten2([], [])
 Call:lists:append([d], [], _6196)
 Exit:lists:append([d], [], [d])
 Exit:flatten2([d], [d])
 Call:flatten2([[], e], _6214)
 Call:flatten2([], _6216)
 Exit:flatten2([], [])
 Call:flatten2([e], _6218)
 Call:flatten2(e, _6220)
 Exit:flatten2(e, [e])
 Call:flatten2([], _6228)
 Exit:flatten2([], [])
 Call:lists:append([e], [], _6218)
 Exit:lists:append([e], [], [e])
 Exit:flatten2([e], [e])
 Call:lists:append([], [e], _6214)
 Exit:lists:append([], [e], [e])
 Exit:flatten2([[], e], [e])
 Call:lists:append([d], [e], _6194)
 Exit:lists:append([d], [e], [d, e])
 Exit:flatten2([[d], [], e], [d, e])
 Call:flatten2([], _6242)
 Exit:flatten2([], [])
 Call:lists:append([d, e], [], _6192)
 Exit:lists:append([d, e], [], [d, e])
 Exit:flatten2([[[d], [], e]], [d, e])
 Call:lists:append([b, c], [d, e], _6156)
 Exit:lists:append([b, c], [d, e], [b, c, d, e])
 Exit:flatten2([[b, c], [[d], [], e]], [b, c, d, e])
 Call:lists:append([a], [b, c, d, e], [a, b, c, d, e])
 Exit:lists:append([a], [b, c, d, e], [a, b, c, d, e])
 Exit:flatten2([a, [b, c], [[d], [], e]], [a, b, c, d, e])

```

## Advanced Projects

3. Write procedures to invert and multiply matrices


### Matrix multiplication

```
:- module(matrix_multiply,
    [matrix_multiply/3
    ,dot_product/3
    ]).
:- use_module(library(clpfd), [transpose/2]).

matrix_multiply(X,Y,M) :-
    transpose(Y,T),
    maplist(row_multiply(T),X,M).

row_multiply(T,X,M) :-
    maplist(dot_product(X),T,M).

dot_product([X|Xs],[T|Ts],M) :-
    foldl(mul,Xs,Ts,X*T,M).
mul(X,T,M,M+X*T).

```

Usage:

```
?- matrix_multiply([[1,2],[3,4],[5,6]], [[1,1,1],[1,1,1]],R),maplist(maplist(is),C,R).
R = [[1*1+2*1, 1*1+2*1, 1*1+2*1], [3*1+4*1, 3*1+4*1, 3*1+4*1], [5*1+6*1, 5*1+6*1, 5*1+6*1]],
C = [[3, 3, 3], [7, 7, 7], [11, 11, 11]].

The numeric evaluation is explicitly requested by , maplist(maplist(is),C,R). R holds the symbolic expressions, C the values.

```


This procedure uses modules from clpfd library : 

a)transpose(+Matrix, ?Transpose)

Transpose a list of lists of the same length. Example:
?- transpose([[1,2,3],[4,5,6],[7,8,9]], Ts).
Ts = [[1, 4, 7], [2, 5, 8], [3, 6, 9]].

b)foldl

```
foldl(:Goal, +List, +V0, -V)
foldl(:Goal, +List1, +List2, +V0, -V)
foldl(:Goal, +List1, +List2, +List3, +V0, -V)
foldl(:Goal, +List1, +List2, +List3, +List4, +V0, -V)
foldl(Goal, List, Start, End) is true if and only if End is the result of applying Goal to the elements of List, 
from left to right, in a cumulative way, using Start as initial accumulator.

```

Call trace for example above:
```

Call:matrix_multiply([[1, 2], [3, 4], [5, 6]], [[1, 1, 1], [1, 1, 1]], _6842)
 Call:clpfd:transpose([[1, 1, 1], [1, 1, 1]], _7292)
 Exit:clpfd:transpose([[1, 1, 1], [1, 1, 1]], [[1, 1], [1, 1], [1, 1]])
 Call:'__aux_maplist/3_row_multiply+1'([[1, 2], [3, 4], [5, 6]], _6842, [[1, 1], [1, 1], [1, 1]])
 Call:row_multiply([[1, 1], [1, 1], [1, 1]], [1, 2], _7410)
 Call:'__aux_maplist/3_dot_product+1'([[1, 1], [1, 1], [1, 1]], _7410, [1, 2])
 Call:dot_product([1, 2], [1, 1], _7416)
 Call:apply:foldl(mul, [2], [1], 1*1, _7416)
 Exit:apply:foldl('1c1aaf8a-66d2-4e9f-a711-4ded80d81b78' : mul, [2], [1], 1*1, 1*1+2*1)
 Exit:dot_product([1, 2], [1, 1], 1*1+2*1)
 Call:'__aux_maplist/3_dot_product+1'([[1, 1], [1, 1]], _7418, [1, 2])
 Call:dot_product([1, 2], [1, 1], _7448)
 Call:apply:foldl(mul, [2], [1], 1*1, _7448)
 Exit:apply:foldl('1c1aaf8a-66d2-4e9f-a711-4ded80d81b78' : mul, [2], [1], 1*1, 1*1+2*1)
 Exit:dot_product([1, 2], [1, 1], 1*1+2*1)
 Call:'__aux_maplist/3_dot_product+1'([[1, 1]], _7450, [1, 2])
 Call:dot_product([1, 2], [1, 1], _7480)
 Call:apply:foldl(mul, [2], [1], 1*1, _7480)
 Exit:apply:foldl('1c1aaf8a-66d2-4e9f-a711-4ded80d81b78' : mul, [2], [1], 1*1, 1*1+2*1)
 Exit:dot_product([1, 2], [1, 1], 1*1+2*1)
 Call:'__aux_maplist/3_dot_product+1'([], _7482, [1, 2])
 Exit:'__aux_maplist/3_dot_product+1'([], [], [1, 2])
 Exit:'__aux_maplist/3_dot_product+1'([[1, 1]], [1*1+2*1], [1, 2])
 Exit:'__aux_maplist/3_dot_product+1'([[1, 1], [1, 1]], [1*1+2*1, 1*1+2*1], [1, 2])
 Exit:'__aux_maplist/3_dot_product+1'([[1, 1], [1, 1], [1, 1]], [1*1+2*1, 1*1+2*1, 1*1+2*1], [1, 2])
 Exit:row_multiply([[1, 1], [1, 1], [1, 1]], [1, 2], [1*1+2*1, 1*1+2*1, 1*1+2*1])
 Call:'__aux_maplist/3_row_multiply+1'([[3, 4], [5, 6]], _7412, [[1, 1], [1, 1], [1, 1]])
 Call:row_multiply([[1, 1], [1, 1], [1, 1]], [3, 4], _7512)
 Call:'__aux_maplist/3_dot_product+1'([[1, 1], [1, 1], [1, 1]], _7512, [3, 4])
 Call:dot_product([3, 4], [1, 1], _7518)
 Call:apply:foldl(mul, [4], [1], 3*1, _7518)
 Exit:apply:foldl('1c1aaf8a-66d2-4e9f-a711-4ded80d81b78' : mul, [4], [1], 3*1, 3*1+4*1)
 Exit:dot_product([3, 4], [1, 1], 3*1+4*1)
 Call:'__aux_maplist/3_dot_product+1'([[1, 1], [1, 1]], _7520, [3, 4])
 Call:dot_product([3, 4], [1, 1], _7550)
 Call:apply:foldl(mul, [4], [1], 3*1, _7550)
 Exit:apply:foldl('1c1aaf8a-66d2-4e9f-a711-4ded80d81b78' : mul, [4], [1], 3*1, 3*1+4*1)
 Exit:dot_product([3, 4], [1, 1], 3*1+4*1)
 Call:'__aux_maplist/3_dot_product+1'([[1, 1]], _7552, [3, 4])
 Call:dot_product([3, 4], [1, 1], _7582)
 Call:apply:foldl(mul, [4], [1], 3*1, _7582)
 Exit:apply:foldl('1c1aaf8a-66d2-4e9f-a711-4ded80d81b78' : mul, [4], [1], 3*1, 3*1+4*1)
 Exit:dot_product([3, 4], [1, 1], 3*1+4*1)
 Call:'__aux_maplist/3_dot_product+1'([], _7584, [3, 4])
 Exit:'__aux_maplist/3_dot_product+1'([], [], [3, 4])
 Exit:'__aux_maplist/3_dot_product+1'([[1, 1]], [3*1+4*1], [3, 4])
 Exit:'__aux_maplist/3_dot_product+1'([[1, 1], [1, 1]], [3*1+4*1, 3*1+4*1], [3, 4])
 Exit:'__aux_maplist/3_dot_product+1'([[1, 1], [1, 1], [1, 1]], [3*1+4*1, 3*1+4*1, 3*1+4*1], [3, 4])
 Exit:row_multiply([[1, 1], [1, 1], [1, 1]], [3, 4], [3*1+4*1, 3*1+4*1, 3*1+4*1])
 Call:'__aux_maplist/3_row_multiply+1'([[5, 6]], _7514, [[1, 1], [1, 1], [1, 1]])
 Call:row_multiply([[1, 1], [1, 1], [1, 1]], [5, 6], _7614)
 Call:'__aux_maplist/3_dot_product+1'([[1, 1], [1, 1], [1, 1]], _7614, [5, 6])
 Call:dot_product([5, 6], [1, 1], _7620)
 Call:apply:foldl(mul, [6], [1], 5*1, _7620)
 Exit:apply:foldl('1c1aaf8a-66d2-4e9f-a711-4ded80d81b78' : mul, [6], [1], 5*1, 5*1+6*1)
 Exit:dot_product([5, 6], [1, 1], 5*1+6*1)
 Call:'__aux_maplist/3_dot_product+1'([[1, 1], [1, 1]], _7622, [5, 6])
 Call:dot_product([5, 6], [1, 1], _7652)
 Call:apply:foldl(mul, [6], [1], 5*1, _7652)
 Exit:apply:foldl('1c1aaf8a-66d2-4e9f-a711-4ded80d81b78' : mul, [6], [1], 5*1, 5*1+6*1)
 Exit:dot_product([5, 6], [1, 1], 5*1+6*1)
 Call:'__aux_maplist/3_dot_product+1'([[1, 1]], _7654, [5, 6])
 Call:dot_product([5, 6], [1, 1], _7684)
 Call:apply:foldl(mul, [6], [1], 5*1, _7684)
 Exit:apply:foldl('1c1aaf8a-66d2-4e9f-a711-4ded80d81b78' : mul, [6], [1], 5*1, 5*1+6*1)
 Exit:dot_product([5, 6], [1, 1], 5*1+6*1)
 Call:'__aux_maplist/3_dot_product+1'([], _7686, [5, 6])
 Exit:'__aux_maplist/3_dot_product+1'([], [], [5, 6])
 Exit:'__aux_maplist/3_dot_product+1'([[1, 1]], [5*1+6*1], [5, 6])
 Exit:'__aux_maplist/3_dot_product+1'([[1, 1], [1, 1]], [5*1+6*1, 5*1+6*1], [5, 6])
 Exit:'__aux_maplist/3_dot_product+1'([[1, 1], [1, 1], [1, 1]], [5*1+6*1, 5*1+6*1, 5*1+6*1], [5, 6])
 Exit:row_multiply([[1, 1], [1, 1], [1, 1]], [5, 6], [5*1+6*1, 5*1+6*1, 5*1+6*1])
 Call:'__aux_maplist/3_row_multiply+1'([], _7616, [[1, 1], [1, 1], [1, 1]])
 Exit:'__aux_maplist/3_row_multiply+1'([], [], [[1, 1], [1, 1], [1, 1]])
 Exit:'__aux_maplist/3_row_multiply+1'([[5, 6]], [[5*1+6*1, 5*1+6*1, 5*1+6*1]], [[1, 1], [1, 1], [1, 1]])
 Exit:'__aux_maplist/3_row_multiply+1'([[3, 4], [5, 6]], [[3*1+4*1, 3*1+4*1, 3*1+4*1], [5*1+6*1, 5*1+6*1, 5*1+6*1]], [[1, 1], [1, 1], [1, 1]])
 Exit:'__aux_maplist/3_row_multiply+1'([[1, 2], [3, 4], [5, 6]], [[1*1+2*1, 1*1+2*1, 1*1+2*1], [3*1+4*1, 3*1+4*1, 3*1+4*1], [5*1+6*1, 5*1+6*1, 5*1+6*1]], [[1, 1], [1, 1], [1, 1]])
 Exit:matrix_multiply([[1, 2], [3, 4], [5, 6]], [[1, 1, 1], [1, 1, 1]], [[1*1+2*1, 1*1+2*1, 1*1+2*1], [3*1+4*1, 3*1+4*1, 3*1+4*1], [5*1+6*1, 5*1+6*1, 5*1+6*1]])
 Call:apply:maplist(maplist((is)), _6822, [[1*1+2*1, 1*1+2*1, 1*1+2*1], [3*1+4*1, 3*1+4*1, 3*1+4*1], [5*1+6*1, 5*1+6*1, 5*1+6*1]])
 Exit:apply:maplist('1c1aaf8a-66d2-4e9f-a711-4ded80d81b78' : maplist((is)), [[3, 3, 3], [7, 7, 7], [11, 11, 11]], [[1*1+2*1, 1*1+2*1, 1*1+2*1], [3*1+4*1, 3*1+4*1, 3*1+4*1], [5*1+6*1, 5*1+6*1, 5*1+6*1]])

```

### Matrix Inversion

```
invert([[A,B],[C,D]], [[IA,IB],[IC,ID]]) :-
    Det is A*D-B*C,
    Det \= 0,
    IDet is 1/Det,
    IA is IDet*D, IB is IDet*(-B),
    IC is IDet*(-C), ID is IDet*A.

```

Matrix inversion for 2x2 matrix

Formula:

[2x2Formula](https://github.com/2XG-DEV/PrologProjects/blob/main/2x2InverseFormula.PNG?raw=true)


test Det \\= 0  , because not every matrix has an inverse.

Usage:

```
invert([[4,7],[2,6]],X)
X = [[0.6000000000000001, -0.7000000000000001], [-0.2, 0.4]]

```

Call trace for above example:

```
Call:invert([[4, 7], [2, 6]], _7092)
 Call:_7550 is 4*6-7*2
 Exit:10 is 4*6-7*2
 Call:10\=0
 Exit:10\=0
 Call:_7570 is 1/10
 Exit:0.1 is 1/10
 Call:_7522 is 0.1*6
 Exit:0.6000000000000001 is 0.1*6
 Call:_7528 is 0.1*-7
 Exit:-0.7000000000000001 is 0.1*-7
 Call:_7540 is 0.1*-2
 Exit:-0.2 is 0.1*-2
 Call:_7546 is 0.1*4
 Exit:0.4 is 0.1*4
 Exit:invert([[4, 7], [2, 6]], [[0.6000000000000001, -0.7000000000000001], [-0.2, 0.4]])

```




