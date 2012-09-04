module listops.
append nil L L.
append (X :: L) K (X :: M) :- append L K M.

reverse L K :-
  pi L\(rev nil L L) =>
  pi X\(pi L\(pi K\(pi M\(rev (X::L) K M :- rev L K (X::M)))))
    => rev L K nil.

rev L K nil :- rev L K (X::M) => rev (X::L) K M
