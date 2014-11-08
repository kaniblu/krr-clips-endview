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
	(slot empty)
	(message-handler init-board)
	(message-handler element-at)
	(message-handler update-at)
	(message-handler print)
	(message-handler get-size)
	(message-handler solve)
	(message-handler verify-solution))

(defmessage-handler EndView load-problem (?x ?y ?alphabets ?top ?left ?right ?bottom ?empty) 
	(if (or (neq (send ?top get-size) ?x) (neq (send ?bottom get-size) ?x) (neq (send ?left get-size) ?y) (neq (send ?right get-size) ?y)) then
		(printout t "ERR: sanity check failed in init-problem" crlf)
		(return))
	(make-instance board of Array)
	(loop-for-count ?y do
		(bind ?row (make-instance of Array))
		(loop-for-count ?x do
			(send ?row push-back ?self:empty))
		(send [board] push-back ?row))
	(bind ?self:board [board])
	(bind ?self:size-x ?x)
	(bind ?self:size-y ?y)
	(bind ?self:top ?top)
	(bind ?self:bottom ?bottom)
	(bind ?self:left ?left)
	(bind ?self:right ?right)
	(bind ?self:empty ?empty)
	(return ?self))

(defmessage-handler EndView load-problem (?matrix ?alphabets ?empty)
	(bind ?matrix-size (send ?matrix get-size))
	(bind ?self:size-x (- (nth$ 1 ?matrix-size) 2))
	(bind ?self:size-y (- (nth$ 2 ?matrix-size) 2))
	(bind ?self:empty ?empty)
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
		(if (eq ?element ?self:empty) then
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
		(if (neq ?given ?self:empty) then
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

(defmessage-handler EndView init-candidates ()
	(bind ?candidates (make-instance of Matrix))
	(send ?candidates init-matrix size-x size-y Nil)
	(loop-for-count (?i 1 ?self:size-x) do
		(loop-for-count (?j 1 ?self:size-y) do
			(send ?candidates update-at ?i ?j (make-instance of Array))))
	(loop-for-count (?i 1 ?self:size-x) do
		(loop-for-count (?j 1 ?self:size-y) do
			(loop-for-count (?k 1 (send ?self:alphabets get-size)) do
				(send (send ?candidates element-at ?i ?j) push-back (send ?self:alphabets element-at ?k)))))
	(loop-for-count (?i 1 ?self:size-x) do
		(bind ?c (send ?self:top element-at ?i))
		(if (neq ?c ?self:empty) then
			(loop-for-count (?j (+ (- ?self:size-y (send ?self:alphabets get-size)) 1) ?self:size-y) do
				(send (send ?candidates element-at ?i ?j) remove ?c))))
	(loop-for-count (?i 1 ?self:size-x) do
		(bind ?c (send ?self:bottom element-at ?i))
		(if (neq ?c ?self:empty) then
			(loop-for-count (?j 1 (- ?self:size-y (send ?self:alphabets get-size))) do
				(send (send ?candidates element-at ?i ?j) remove ?c))))
	(loop-for-count (?i 1 ?self:size-y) do
		(bind ?c (send ?self:left element-at ?i))
		(if (neq ?c ?self:empty) then
			(loop-for-count (?j (+ (- ?self:size-x (send ?self:alphabets get-size)) 1) ?self:size-x) do
				(send (send ?candidates element-at ?i ?j) remove ?c))))
	(loop-for-count (?i 1 ?self:size-y) do
		(bind ?c (send ?self:right element-at ?i))
		(if (neq ?c ?self:empty) then
			(loop-for-count (?j 1 (- ?self:size-x (send ?self:alphabets get-size))) do
				(send (send ?candidates element-at ?i ?j) remove ?c))))
	(return ?candidates))

(defmessage-handler EndView solve ()
	(bind ?candidates (send ?self init-candidates)))
