module reverse.

reverse L1 L2 :-
   (pi rev_aux \
      ((rev_aux nil L2,
        (pi X\ pi L1\ pi L2\ (rev_aux (X::L1) L2 :- rev_aux L1 (X::L2))))
          => (rev_aux L1 nil))).

reverse1 [X] [X].
reverse1 [X|L1] L2 :- reverse1 L1 [X|L2].
