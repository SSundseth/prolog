/*
    ssundseth Comp360 final project. Game of Life.

    Prolog Implementation of Conway's Game of Life. Rules of Life:
    1) A cell with more than 2 or less than 3 neighbors dies
    2) A cell with 2 or 3 neighbors remains alive
    3) A dead cell with 3 neighbors becomes alive

    To Run:
        life(Size, Pause).  -- Will create a Size x Size board with a random
                               dispersion of live cells. Pause determines the
                               time between generations.

    Some Examples (Run by querying them in the interpreter):


    blinker --     @ @ @


                   @ @
    glider --      @   @
                   @



                        @ @
    r_pentomino --       @ @
                          @


   ten_in_a_row --  @ @ @ @ @ @ @ @ @ @


                    @
                  @ @ @
   pulsar --      @   @
                  @ @ @
                    @

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%High Level Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Main predicate. Pause must be a float and Matrix must be a list of lists, all
%of length Size, filled with 0's and 1's
run(Size, Pause, Matrix) :-
    showBoard(Matrix),
    nl,
    nl,
    pause(Pause),
    update(Matrix, Size, 0,0, Matrix, NewMatrix),
    run(Size, Pause, NewMatrix).

%Create and run a Size x Size matrix with sparsity of 20%
life(Size, Pause) :-
    sparseSquareMatrix(Size, Matrix),
    run(Size, Pause, Matrix).

%display the board with '@' as alive and '_' as dead cells
showBoard([]).
showBoard([H|T]) :-
    showRow(H),
    showBoard(T).

showRow([]) :- nl.
showRow([H|T]) :-
    showCell(H),
    showRow(T).

showCell(0) :- write(' _').
showCell(1) :- write(' @').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Game Engine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Creates the matrix for the next generation
update(Matrix, Size, Size, Col, Accum, Accum) :- !.
update(Matrix, Size, Row, Size, Accum, NewMatrix) :-
    Row < Size,
    Row1 is Row+1, !,
    update(Matrix, Size, Row1, 0, Accum, NewMatrix).
update(Matrix, Size, Row, Col, Accum, NewMatrix) :-
    sumNeighbors(Size, Matrix, Row, Col, N),
    auxUpdate(Matrix, Row, Col, N, Accum, Mid),
    Col < Size,
    Col1 is Col+1,
    update(Matrix, Size, Row, Col1, Mid, NewMatrix).

%2 Neighbors: nothing happens
auxUpdate(Matrix, Row, Col, 2, Accum, Accum) :- !.

%Rule (2): cell remains alive with 3 neighbors
auxUpdate(Matrix, Row, Col, 3, Accum, NewMatrix) :-
    mSelect(Matrix, Row, Col, 1),
    mReplace(Accum, Row, Col, 1, NewMatrix), !.

%Rule (3): dead cell with 3 neighbors is born
auxUpdate(Matrix, Row, Col, 3, Accum, NewMatrix) :-
    mSelect(Matrix, Row, Col, 0),
    mReplace(Accum, Row, Col, 1, NewMatrix), !.

%Rule (1): Cell with <2 or >3 neighbors dies or stays dead
auxUpdate(Matrix, Row, Col, N, Accum, NewMatrix) :-
    N \== 2,
    N \== 3,!,
    mReplace(Accum, Row, Col, 0, NewMatrix).


%Replace the value at (Row, Col) in Matrix with NewVal
mReplace(Matrix, Row, Col, NewVal, NewMatrix) :-
    auxMReplace(Matrix, Row, 0, Col, NewVal, [], NewMatrix), !.

auxMReplace([[H|T]|T2], Row, RCount, Col, NewVal, Accum, NewMatrix) :-
    RCount < Row,
    RCount1 is RCount+1,
    append(Accum, [[H|T]], NewAccum),
    auxMReplace(T2, Row, RCount1, Col, NewVal, NewAccum, NewMatrix).
auxMReplace([[H|T]|T2], Row, Row, Col, NewVal, Accum, NewMatrix) :-
    replace([H|T], Col, NewVal, NewRow),
    append(Accum, [NewRow|T2], NewMatrix).

%Helper for mReplace. Replace the value at the Nth element of List with NewVal
replace(List, N, NewVal, NewList) :-
    auxReplace(List, N, 0, NewVal, [], NewList), !.

auxReplace([H|T], N, Count, NewVal, Accum, NewList) :-
    Count < N,
    Count1 is Count+1,
    append(Accum, [H], NewAccum),
    auxReplace(T, N, Count1, NewVal, NewAccum, NewList).
auxReplace([H|T], N, N, NewVal, Accum, NewList) :-
    append(Accum, [NewVal|T], NewList).

%Count the neighbors of (R,C) in Matrix. Wraps around so that the board is
%a torus
sumNeighbors(Size, Matrix, R, C, N) :-
    myMod(R-1, Size, R1),myMod(R+1, Size, R2),
    myMod(C-1, Size, C1),myMod(C+1, Size, C2),
    mSelect(Matrix,R1,C1,N1),mSelect(Matrix,R1,C,N2),mSelect(Matrix,R1,C2,N3),
    mSelect(Matrix, R, C1, N4),                    mSelect(Matrix, R, C2, N5),
    mSelect(Matrix,R2,C1,N6),mSelect(Matrix,R2,C,N7),mSelect(Matrix,R2,C2,N8),
    sumList([N1,N2,N3,N4,N5,N6,N7,N8], N), !.

%Helper for sumNeighbors. Count how many elements of the first arg are 1's
sumList([], 0).
sumList([0|T], N) :-
    sumList(T, N).
sumList([1|T], N) :-
    sumList(T, N1),
    N is N1+1, !.

%Get the value at (Row, Col) in Matrix
mSelect(Matrix, Row, Col, Item) :-
    auxMSelect(Matrix, Row, 0, Col, Item).

auxMSelect([R|M], Row, Row, Col, Item) :-
    select(R, Col, Item), !.
auxMSelect([[H|T]|T2], Row, RCount, Col, Item) :-
    RCount < Row,
    RCount1 is RCount+1,
    auxMSelect(T2, Row, RCount1, Col, Item).

%Helper for mSelect. Get the value at N in List
select(List, N, Item) :-
    auxSelect(List, N, 0, Item), !.

auxSelect([H|T], N, N, H).
auxSelect([H|T], N, Count, Item) :-
    Count < N,
    Count1 is Count+1,
    auxSelect(T, N, Count1, Item).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Helpers/Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Prolog built in mod defines -1 mod X = -1 when X > 0. We need it to be X-1
%to allow for wrapping of the 0th row and column
myMod(X, Y, Z) :- X >= 0, Z is X mod Y.
myMod(X, Y, Z) :- X < 0, Z is Y+X.

%pause the running of the program for X seconds, X must be a float
pause(X) :- get_time(T),
    pause(T,T1,X).

pause(T,T1,W) :-
    repeat,
    get_time(T1),
    W1 is T1-T,
    W1 > W.

%Fill a list of length N with 0's and 1's with 20% sparsity
sparseRandomFill(0,[]).
sparseRandomFill(N,[H|Tl]) :- H is random(10)//8,
    N1 is N-1,
    sparseRandomFill(N1,Tl).

%Create an N x N matrix with 20% sparsity
sparseSquareMatrix(N,M) :-
    N2 is N*N,
    sparseRandomFill(N2,L),
    clump(N,L,M),!.

%Gets the length of a list
findLength([],0).
findLength([H|T],Length) :- findLength(T,Tlength),Length is Tlength+1.


takeN(0,X,Count,Count,X).
takeN(Accum,[H|T],Temp,NewR,Rem) :-
    Accum1 is Accum-1,
    append(Temp, [H], NewTemp),
    takeN(Accum1,T,NewTemp,NewR,Rem).

aux_clump(Count,L,Temp,M) :-
    findLength(L,Len), Len=<Count,
    append(Temp,[L],M).
aux_clump(Count,L,Temp,M) :-
    findLength(L,Len), Len>Count,
    takeN(Count,L,[],NewR,Rem),
    append(Temp,[NewR],NewTemp),
    aux_clump(Count,Rem,NewTemp,M).

clump(N,L,M) :-
    aux_clump(N,L,[],M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Examples
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

blinker :- run(5, 0.5,
               [[0,0,0,0,0],[0,0,0,0,0],[0,1,1,1,0],[0,0,0,0,0],[0,0,0,0,0]]).

glider :- run(19, 0.25,
    [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]).

r_pentomino :- run(19, 0.5,
    [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]).

ten_in_a_row :- run(19, 0.5,
    [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]).


pulsar :- run(19, 0.5,
    [[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
     [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]).
