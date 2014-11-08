(defclass Matrix "a generic two-dimensional array"
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
	
