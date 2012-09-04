sig felty.

kind person     type.

type mappred    (A -> B -> o) -> list A -> list B -> o.
type forevery   (A -> o) -> list A -> o.
type forsome    (A -> o) -> list A -> o.
type sublist    (A -> o) -> list A -> list A -> o.
type age        person -> int -> o.

type bob        person.
type ned        person.
type susan      person.

kind insect, jar        type.

type i                  insect.
type j                  jar.
type sterile, heated    jar -> o.
type dead, bug          insect -> o.
type in                 insect -> jar -> o.
