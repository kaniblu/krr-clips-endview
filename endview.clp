(defclass Array "a representation of a row in a board"
	(is-a USER)
	(role concrete)
	(multislot array (default (create$)))
	(message-handler init-array)
	(message-handler element-at)
	(message-handler update-at)
	(message-handler get-size)
	(message-handler push-back)
	(message-handler contains)
	(message-handler subarray))

(defmessage-handler Array init-array (?size ?value)
	(bind $?tmp (create$))
	(loop-for-count ?size do
		(bind $?tmp (insert$ $?tmp 1 ?value)))
	(bind $?self:array $?tmp))

(defmessage-handler Array element-at (?index)
	(nth$ ?index $?self:array))

(defmessage-handler Array update-at (?index ?new-value)
	(bind $?self:array (insert$ (delete$ $?self:array ?index ?index) ?index ?new-value)))

(defmessage-handler Array get-size ()
	(length$ $?self:array))

(defmessage-handler Array push-back (?x)
	(bind $?self:array (insert$ $?self:array (+ (length$ $?self:array) 1) ?x)))

(defmessage-handler Array contains (?x)
	(return (neq (member$ ?x $?self:array) FALSE)))

(defmessage-handler Array subarray (?start ?end)
	(bind ?new-array (make-instance of Array))
	(loop-for-count (?i ?start ?end) do	
		(send ?new-array push-back (send ?self element-at ?i)))
	(return ?new-array))

(defclass Matrix "two-dimensional array"
	(is-a USER)
	(role concrete)
	(slot matrix)
	(slot size-x)
	(slot size-y)
	(message-handler init-matrix)
	(message-handler element-at)
	(message-handler update-at)
	(message-handler get-size)
	(message-handler check-coord)
	(message-handler submatrix)
	(message-handler print))
	
(defmessage-handler Matrix print ()
	(loop-for-count (?i 1 ?self:size-y) do
		(loop-for-count (?j 1 ?self:size-x) do
			(printout t (send ?self element-at ?j ?i))
			(printout t " "))
		(printout t crlf)))

(defmessage-handler Matrix check-coord (?x ?y)
	(bind ?res (and (> ?x 0) (> ?y 0) (<= ?x ?self:size-x) (<= ?y ?self:size-y)))
	(if (not ?res) then
		(printout t "ERR: coord sanity check failed." crlf))
	(return ?res))

(defmessage-handler Matrix init after ()
	(bind ?self:matrix (make-instance of Array)))
	
(defmessage-handler Matrix init-matrix (?x ?y ?value)
	(bind ?self:size-x ?x)
	(bind ?self:size-y ?y)
	(loop-for-count ?y do
		(bind ?row (make-instance of Array))
		(send ?row init-array ?x ?value)
		(send ?self:matrix push-back ?row))
	(return ?self))
	
(defmessage-handler Matrix element-at (?x ?y)
	(if (not (send ?self check-coord ?x ?y)) then
		(return))
	(return (send (send ?self:matrix element-at ?y) element-at ?x)))

(defmessage-handler Matrix update-at (?x ?y ?new-value)
	(if (not (send ?self check-coord ?x ?y)) then
		(return))
	(send (send ?self:matrix element-at ?y) update-at ?x ?new-value)
	(return ?self))
	
(defmessage-handler Matrix get-size ()
	(create$ ?self:size-x ?self:size-y))

(defmessage-handler Matrix submatrix (?start-x ?end-x ?start-y ?end-y)
	(bind ?new-matrix (make-instance of Matrix))
	(bind ?size-x (+ (- ?end-x ?start-x) 1))
	(bind ?size-y (+ (- ?end-y ?start-y) 1))
	(send ?new-matrix init-matrix ?size-x ?size-y nil)
	(loop-for-count (?i ?start-x ?end-x) do
		(loop-for-count (?j ?start-y ?end-y) do
			(send ?new-matrix update-at (+ (- ?i ?start-x) 1) (+ (- ?j ?start-y) 1) (send ?self element-at ?i ?j))))
	(return ?new-matrix))
	
(deffunction matrix (?x ?y ?array2d)
	(bind ?new-matrix (make-instance of Matrix))
	(send ?new-matrix init-matrix ?x ?y nil)
	(loop-for-count (?i 1 ?y) do
		(loop-for-count (?j 1 ?x) do
			(send ?new-matrix update-at ?j ?i (send (send ?array2d element-at ?i) element-at ?j))))
	(return ?new-matrix))
	
(deffunction array ($?args)
	(bind ?arr (make-instance of Array))
	(loop-for-count (?i 1 (length$ $?args)) do
		(send ?arr push-back (nth$ ?i $?args)))
	(return ?arr))

(defclass EndView "an EndView Puzzle"
	(is-a USER)
	(role concrete)
	(slot board (storage local))
	(slot size-x)
	(slot size-y)
	(slot alphabets)
	(slot top)
	(slot left)
	(slot right)
	(slot bottom)
	(message-handler init-board)
	(message-handler element-at)
	(message-handler update-at)
	(message-handler print)
	(message-handler get-size)
	(message-handler solve)
	(message-handler verify-solution))

(defmessage-handler EndView load-problem (?x ?y ?alphabets ?top ?left ?right ?bottom) 
	(if (or (neq (send ?top get-size) ?x) (neq (send ?bottom get-size) ?x) (neq (send ?left get-size) ?y) (neq (send ?right get-size) ?y)) then
		(printout t "ERR: sanity check failed in init-problem" crlf)
		(return))
	(make-instance board of Array)
	(loop-for-count ?y do
		(bind ?row (make-instance of Array))
		(loop-for-count ?x do
			(send ?row push-back _))
		(send [board] push-back ?row))
	(bind ?self:board [board])
	(bind ?self:size-x ?x)
	(bind ?self:size-y ?y)
	(bind ?self:top ?top)
	(bind ?self:bottom ?bottom)
	(bind ?self:left ?left)
	(bind ?self:right ?right)
	(return ?self))

(defmessage-handler EndView load-problem (?matrix ?alphabets)
	(bind ?matrix-size (send ?matrix get-size))
	(bind ?self:size-x (- (nth$ 1 ?matrix-size) 2))
	(bind ?self:size-y (- (nth$ 2 ?matrix-size) 2))
	(bind ?self:top (make-instance of Array))
	(bind ?self:bottom (make-instance of Array))
	(bind ?self:left (make-instance of Array))
	(bind ?self:right (make-instance of Array))
	(bind ?self:board (send ?matrix submatrix 2 (+ 1 ?self:size-x) 2 (+ 1 ?self:size-y)))
	(bind ?self:alphabets ?alphabets)
	(loop-for-count (?i 2 (+ ?self:size-y 1)) do
		(send ?self:left push-back (send ?matrix element-at 1 ?i)))
	(loop-for-count (?i 2 (+ ?self:size-y 1)) do
		(send ?self:right push-back (send ?matrix element-at (+ ?self:size-x 2) ?i)))
	(loop-for-count (?i 2 (+ ?self:size-x 1)) do
		(send ?self:top push-back (send ?matrix element-at ?i 1)))
	(loop-for-count (?i 2 (+ ?self:size-x 1)) do
		(send ?self:bottom push-back (send ?matrix element-at ?i (+ ?self:size-x 2))))
	(return ?self))

(defmessage-handler EndView print ()
	(printout t "+ | ")
	(loop-for-count (?i 1 ?self:size-x) do
		(printout t (send ?self:top element-at ?i))
		(printout t " "))
	(printout t "| + " crlf)
	(loop-for-count (+ (* ?self:size-x 2) 8) do
		(printout t "-"))
	(printout t crlf)
	(loop-for-count (?i 1 ?self:size-y) do
		(printout t (send ?self:left element-at ?i))
		(printout t " | ")
		(loop-for-count (?j 1 ?self:size-x) do
			(printout t (send ?self element-at ?j ?i))
			(printout t " "))
		(printout t "| ")
		(printout t (send ?self:right element-at ?i))
		(printout t " ")
		(printout t crlf))
	(loop-for-count (+ (* ?self:size-x 2) 8) do
		(printout t "-"))
	(printout t crlf)
	(printout t "+ | ")
	(loop-for-count (?i 1 ?self:size-x) do
		(printout t (send ?self:bottom element-at ?i))
		(printout t " "))
	(printout t "| + " crlf))

(defmessage-handler EndView element-at (?x ?y)
	(return (send ?self:board element-at ?x ?y)))

(defmessage-handler EndView update-at (?x ?y ?new-value)
	(send ?self:board update-at ?x ?y ?new-value))

(defmessage-handler EndView get-size ()
	(create$ ?self:size-x ?self:size-y))
	
(defmessage-handler EndView verify-given (?target ?start ?change ?end ?constant ?is-x-changing)
	(bind ?i ?start)
	(while (if (< ?change 0) then (>= ?i ?end) else (<= ?i ?end)) do
		(bind ?element (if ?is-x-changing then (send ?self element-at ?i ?constant) else (send ?self element-at ?constant ?i)))
		(if (eq ?element _) then
			(bind ?i (+ ?i ?change))
		else
			(if (eq ?element ?target) then
				(return TRUE)
			else
				(return FALSE))))
	(return FALSE))
	
(defmessage-handler EndView verify-given-array (?given-array ?is-x-changing ?is-reverse)
	(loop-for-count (?i 1 (send ?given-array get-size)) do
		(bind ?given (send ?given-array element-at ?i))
		(if (neq ?given _) then
			(bind ?start (if ?is-x-changing then (if ?is-reverse then ?self:size-x else 1) else (if ?is-reverse then ?self:size-y else 1)))
			(bind ?change (if ?is-reverse then -1 else 1))
			(bind ?end (if ?is-reverse then 1 else (if ?is-x-changing then ?self:size-x else ?self:size-y)))
			(bind ?constant ?i)
			(bind ?result (send ?self verify-given ?given ?start ?change ?end ?constant (not ?is-x-changing)))
			(if (not ?result) then
				(return FALSE))))
	(return TRUE))
	
(defmessage-handler EndView verify-constraint1 ()
	(return (and (send ?self verify-given-array ?self:top TRUE FALSE)
		(send ?self verify-given-array ?self:left FALSE FALSE)
		(send ?self verify-given-array ?self:right FALSE TRUE)
		(send ?self verify-given-array ?self:bottom TRUE TRUE))))
		
(defmessage-handler EndView verify-0d (?constant ?is-x-changing)
	(bind ?start 1)
	(bind ?end (if ?is-x-changing then ?self:size-x else ?self:size-y))
	(bind ?hashset (make-instance of Array))
	(loop-for-count (?i ?start ?end) do
		(bind ?element (if ?is-x-changing then (send ?self element-at ?i ?constant) else (send ?self element-at ?constant ?i)))
		(if (send ?hashset contains ?element) then
			(unmake-instance ?hashset)
			(return FALSE))
		(send ?hashset push-back ?element))
	(if (neq (send ?hashset get-size) (send ?self:alphabets get-size)) then
		(unmake-instance ?hashset)
		(return FALSE))
	(unmake-instance ?hashset)
	(return TRUE))
	
(defmessage-handler EndView verify-1d (?is-x-changing)
	(bind ?end (if ?is-x-changing then ?self:size-x else ?self:size-y))
	(loop-for-count (?i 1 ?end) do
		(bind ?result (send ?self verify-0d ?i (not ?is-x-changing)))
		(if (not ?result) then
			(return FALSE)))
	(return TRUE))
	
(defmessage-handler EndView verify-constraint2 ()
	(and (send ?self verify-1d TRUE) (send ?self verify-1d FALSE)))
	
(defmessage-handler EndView verify-solution ()
	(return (and (send ?self verify-constraint1) (send ?self verify-constraint2))))

(bind ?ex (matrix 5 5 (array (array _ a _ _ _) (array _ a b c c) (array _ b c a _) (array _ c a b _) (array _ c _ b _))))
