(defclass Array "a generic array like in java"
	(is-a USER)
	(role concrete)
	(multislot array (default (create$)))
	(message-handler init-array)
	(message-handler element-at)
	(message-handler update-at)
	(message-handler get-size)
	(message-handler push-back)
	(message-handler contains)
	(message-handler subarray)
	(message-handler push-back-if-not-exists)
	(message-handler remove))

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

(defmessage-handler Array push-back-if-not-exists (?x)
	(if (not (send ?self contains ?x)) then
		(send ?self push-back ?x)
		(return TRUE)
	else
		(return FALSE)))

(defmessage-handler Array remove (?x)
	(bind ?index (member$ ?x ?self:array))
	(if (not ?index) then
		(return FALSE)
	else
		(delete$ ?self:array ?index ?index)
		(return TRUE)))

(deffunction array ($?args)
	(bind ?arr (make-instance of Array))
	(loop-for-count (?i 1 (length$ $?args)) do
		(send ?arr push-back (nth$ ?i $?args)))
	(return ?arr))


