(deffunction extract-row (?nth ?x ?y $?array) 
	
	"Extracts ?nth row from a ?x by ?y matrix, where
	the matrix is represented as a flattened out array"

	(bind $?new-array (create$))
	(loop-for-count (?i 
						(+ (* ?x (- ?nth 1)) 1) 
						(* ?x ?nth)) do
		(bind $?new-array 
			(insert$ $?new-array 
				(+ 	(length$ $?new-array) 1) 
				(nth$ ?i $?array))))
	(return $?new-array))

(deffunction extract-column (?nth ?x ?y $?array)

	"This function extracts ?nth column from a ?x by ?y matrix,
	where the matrix is represented as a flattened out array."

	(bind $?new-array (create$))
	(bind ?i ?nth)
	(while TRUE do
		(if (> ?i (length$ $?array)) then
			(break))
		(bind $?new-array 
			(insert$ $?new-array 
				(+ (length$ $?new-array) 1) 
				(nth$ ?i $?array)))
		(bind ?i (+ ?i ?y)))
	(return $?new-array))

(deffacts initial 	
	
	"These initial facts contain the problems 
	and the answers for the End View Puzzle, 
	copied from the prolog assignment. The dimensions of puzzle boards
	are specified as well, as they are needed to decode the two dimensional 
	arrays that were flattened out in single dimensional arrays."

	(dimension 1 4 4)
	(puzzle 1 (create$ 	x x x a x x
						x _ _ _ _ x
						c _ _ _ _ x
						c _ _ _ _ b
						x _ _ _ _ x
						x x b x x x))
	(answer 1 (create$	b x a c
						x c b a
						c a x b
						a b c x))
	(alphabet 1 (create$ x a b c))
	
	(dimension 2 5 5)
	(puzzle 2 (create$	x x x x c c x
						b _ _ _ _ _ x
						x _ _ _ _ _ x
						x _ _ _ _ _ d
						d _ _ _ _ _ x
						b _ _ _ _ _ x
						x x x x a x x))
	(answer 2 (create$	b d a x c
						a x d c b
						c a b d x
						d c x b a
						x b c a d))
	(alphabet 2 (create$ x a b c d)))

(defrule decompose
	
	"This rule decomposes puzzles into top,
	left, right, and bottom multifields."
	
	(dimension ?i ?x ?y)
	(puzzle ?i $?p-array)
	(alphabet ?i $?ab-array)
=>
	(bind ?matrix-x (+ ?x 2))
	(bind ?matrix-y (+ ?y 2))
	(assert (top ?i (extract-row 1 ?matrix-x ?matrix-y $?p-array)))
	(assert (bottom ?i (extract-row ?matrix-y ?matrix-x ?matrix-y $?p-array)))
	(assert (left ?i (extract-column 1 ?matrix-x ?matrix-y $?p-array)))
	(assert (right ?i (extract-column ?matrix-x ?matrix-x ?matrix-y $?p-array))))
	(assert (solution ?i  

(defrule 


	(
