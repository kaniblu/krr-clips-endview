(defclass Array "a representation of a row in a board"
	(is-a USER)
	(role concrete)
	(multislot array (default (create$)))
	(message-handler init-array)
	(message-handler element-at)
	(message-handler update-at)
	(message-handler get-size)
	(message-handler push-back)
	(message-handler contains))

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
	(message-handler check-coord)
	(message-handler verify-solution))

(defmessage-handler EndView init-problem (?x ?y ?alphabets ?top ?left ?right ?bottom) 
	(if (or (neq (send ?top get-size) ?x) (neq (send ?bottom get-size) ?x) (neq (send ?left get-size) ?y) (neq (send ?right get-size) ?y)) then
		(printout t "ERR: sanity check failed in init-problem" crlf)
		(return))
	(make-instance board of Array)
	(loop-for-count ?y do
		(make-instance row of Array)
		(loop-for-count ?x do
			(send [row] push-back _))
		(send [board] push-back [row]))
	(bind ?self:board [board])
	(bind ?self:size-x ?x)
	(bind ?self:size-y ?y)
	(bind ?self:top ?top)
	(bind ?self:bottom ?bottom)
	(bind ?self:left ?left)
	(bind ?self:right ?right)
	(return ?self))

(defmessage-handler EndView check-coord (?x ?y)
	(bind ?res (and (> ?x 0) (> ?y 0) (<= ?x ?self:size-x) (<= ?y ?self:size-y)))
	(if (not ?res) then
		(printout t "ERR: coord sanity check failed." crlf))
	(return ?res))

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
		(bind ?row (send ?self:board element-at ?i))
		(loop-for-count (?j 1 (send ?row get-size)) do
			(printout t (send ?row element-at ?j))
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
	(if (not (send ?self check-coord ?x ?y)) then
		(return))
	(send (send ?self:board element-at ?y) element-at ?x))	

(defmessage-handler EndView update-at (?x ?y ?new-value)
	(if (not (send ?self check-coord ?x ?y)) then
		(return))
	(send (send ?self:board element-at ?y) update-at ?x new-value))

(defmessage-handler EndView get-size ()
	(create$ ?self:size-x ?self:size-y))

(defmessage-handler EndView verify-solution ()
	(deffunction verify-constraint1 ()
		(deffunction verify-given (?given ?start ?change ?end ?constant ?is-x-changing)
			(bind ?i ?start)
			(while (<= ?i ?end) do
			(bind ?element (if ?is-x-changing then (send ?self element-at ?i ?constant) else (send ?self element-at ?constant ?i)))
				(if (eq ?element _) then
					(bind ?i (+ ?i ?change))
					(continue))
				(if (eq ?element ?target) then
					(return TRUE)
				else
					(return FALSE)))
			(return FALSE))
		(deffunction verify-given-array (?given-array ?is-x-changing ?is-reverse)
			(loop-for-count (?i 1 (send ?given-array get-size)) do
				(bind ?given (send ?given-array element-at ?i))
				(bind ?start (if ?is-x-changing then (if ?is-reverse then ?self:x-size else 1) else (if ?is-reverse then ?self:y-size else 1)))
				(bind ?change (if ?is-reverse then -1 else 1))
				(bind ?end (if ?is-reverse then 1 else (if ?is-x-changing then ?self:x-size else ?self:y-size)))
				(bind ?constant ?i)
				(bind ?result (verify-given ?given ?start ?change ?end ?constant (not ?is-x-changing)))
				(if (not ?result) then
					(return FALSE)))
			(return TRUE))
		(return (and (verify-given-array ?self:top TRUE FALSE)
			(verify-given-array ?self:left FALSE FALSE)
			(verify-given-array ?self:right FALSE TRUE)
			(verify-given-array ?self:bottom TRUE TRUE))))
	(deffunction verify-constraint2 ()
		(deffunction verify-0d (?constant ?is-x-changing)
			(bind ?start 1)
			(bind ?end (if ?is-x-changing then ?self:x-size else ?self:y-size))
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
		(deffunction verify-1d (?is-x-changing)
			(bind ?end (if ?is-x-changing then ?self:x-size else ?self:y-size))
			(loop-for-count (?i 1 ?end) do
				(bind ?result (verify-0d (?i (not ?is-x-changing))))
				(if (not ?result) then
					(return FALSE)))
			(return TRUE))
		(and (verify-1d TRUE) (verify-1d FALSE)))
	(and (verify-contraint1 ()) (verify-constraint2 ())))	