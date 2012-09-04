module math.


sum i X X.
sum (s X) Y (s Z) :- sum X Y Z.

mult i X i.
mult (s X) Y Z :- mult X Y W, sum W Y Z.

sum1 0 X X.
sum1 (X + 1) Y (Z + 1) :- sum1 X Y Z.

mult1 0 X 0.
mult1 (X + 1) Y Z :- mult1 X Y W, sum1 W Y Z.

sum2 X Y Z :- Z is X + Y.
