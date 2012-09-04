module gol_helpers.

myMod X Y Z :- X >= 0 & Z is X mod Y.
myMod X Y Z :- X < 0 & Z is Y + X.
