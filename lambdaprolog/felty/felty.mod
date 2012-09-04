module felty.

mappred P nil nil.
mappred P (X::L) (Y::K) :- P X Y, mappred P L K.

forevery P nil.
forevery P (X::L) :- P X, forevery P L.

forsome P (X::L) :- P X, forsome P L.

sublist P (X::L) (X::K) :- P X, sublist P L K.
sublist P (X::L) K :- sublist P L K.
sublist P nil nil.

age bob 23.
age ned 23.
age susan 25.


sterile J :- pi x\(bug x => in x J => dead x).
dead B :- heated j, in B j, bug B.
heated j.
