;;;; genkov.lisp

(in-package #:genkov)

;; CONDITIONS

(define-condition not-a-llist (error)
  ((nllist :initarg :nllist
	   :initform nil
	   :reader nllist))
  (:documentation "Custom error: The provided parameter is not a list of lists.")
  (:report (lambda (condition stream)
	     (format stream
		     "~a is not a list of lists." (nllist condition)))))

(define-condition empty-states (error)
  ((estate :initarg :estate
	   :initform nil
	   :reader estate))
  (:documentation "Custom error: The Markov Chain states are empty.")
  (:report (lambda (condition stream)
	     (format stream
		     "The Markov Chain states are ~a.~%Use set-states to populate it." (estate condition)))))

(define-condition not-a-valid-state (error)
  ((nvstate :initarg :nvstate
	    :initform nil
	    :reader nvstate))
  (:documentation "Custom error: The provided parameter is a non-existent state in the current model.")
  (:report (lambda (condition stream)
	     (format stream
		     "~a is a non-existent state in the current model." (nvstate condition)))))

;; LOGIC

(defparameter +model+ nil)
(defparameter +states+ nil)

(defun reset-model ()
  "Reset the Markov Chain model."
  (setf +model+ nil))

(defun get-model ()
  "Return the Markov Chain model."
  +model+)

(defun reset-states ()
  "Reset the Markov Chain states."
  (setf +states+ nil))

(defun get-states ()
  "Return the Markov Chain states."
  +states+)

(defun compute-state-transition-probabilities (states)
  "Compute the probabilities of transitions between states in a list of lists."
  (let ((transitions (make-hash-table :test #'equal))
        (counts (make-hash-table :test #'equal)))
    ;; Build transition counts for each sublist
    (dolist (sublist states)
      (loop for (current next) on sublist
            do (progn
                 (incf (gethash current counts 0)) ;; Count occurrences of each element
                 (when next
                   (incf (gethash (list current next) transitions 0)))))
      ;; Add transitions to nil for last elements in sublists
      (when (not (null sublist))
        (incf (gethash (list (car (last sublist)) nil) transitions 0))))
    ;; Normalize to probabilities
    (let ((results (make-hash-table :test #'equal)))
      (maphash (lambda (transition count)
                 (let ((current (first transition)))
                   (setf (gethash transition results)
                         (/ (float count) (gethash current counts)))))
               transitions)
      results)))

(defun print-model ()
  "Pretty printer for the markov Chain model. Primarily intended as a prototyping/debugging utility, might be useful for someone. Pay attention, this function could print a lot of lines if used for large models."
  (loop :for k :being :the hash-keys :of +model+
	:for v :being :the hash-values :of +model+
	:do
	   (loop :for k2 :being :the hash-keys :of v
		 :for v2 :being :the hash-values :of v
		 :do
		    (format t "~a~t~a~t~a:~t~a~%" k #\u21d2 k2 v2))))

(defun transform-transition-table (input-table)
  "Transforms the given hash table of transitions into a nested hash table."
  (let ((output-table (make-hash-table :test 'equalp)))
    (maphash (lambda (key value)
               (destructuring-bind (start-state end-state) key
                 (let ((nested-table (gethash start-state output-table)))
                   ;; Create a nested ht if key doesn't exist
                   (unless nested-table
                     (setf nested-table (make-hash-table :test 'equal))
                     (setf (gethash start-state output-table) nested-table))
		   ;; Add value to the nested ht
                   (setf (gethash end-state nested-table) value))))
             input-table)
    output-table))

(defun ensure-llist (llist)
  "Check wether llist is a list where each item contained is a list."
  (every #'listp llist))

(defun set-states (states)
  "Populate the Markov Chain states."
  (warn "\"set-states\" is deprecated and will be removed soon. Please switch to \"add-states\"")
  (if (ensure-llist states)
      (setf +states+ states)
      (restart-case
          (error 'not-a-llist :nllist states)
        (encapsulate-into-list ()
          :report "Try encapsulating the input into a single list."
          (set-states (list states))))))

(defun add-states (states)
  "Populate the Markov Chain states or add STATES to the already existing ones."
  (if (ensure-llist states)
      (setf +states+ (concatenate 'list +states+ states))
      (restart-case
          (error 'not-a-llist :nllist states)
        (encapsulate-into-list ()
          :report "Try encapsulating the input into a single list."
          (set-states (list states))))))

(defun generate-model ()
  "Populate the Markov Chain model."
  (if (null +states+)
      (error 'empty-states :estate +states+)
      (setf +model+ (transform-transition-table (compute-state-transition-probabilities +states+)))))

;; GENERATON

(defun select-next-state (init-state)
  "Given an initial state, return the next one based on the probabilities in the MC model."
  (let ((transitions (gethash init-state +model+)))
    (if transitions
        (let* ((cumulative-probs (make-hash-table :test #'equalp))
               (total 0.0))
          ;; Compute cumulative probabilities
          (maphash (lambda (state probability)
                     (setf total (+ total probability))
                     (setf (gethash state cumulative-probs) total))
                   transitions)
          ;; Generate a random number and find the next state
          (let ((rand (random 1.0)))
            (loop :for state :being :the hash-keys :of cumulative-probs
                    :using (hash-value cumulative-prob)
                  :do (when (<= rand cumulative-prob)
			(return state)))))
        nil)))

(defun select-random-state ()
  "Returns a random state from the MC model."
  (let ((keys (loop :for key :being :the hash-keys :of +model+
		    :collect key)))
    (when keys
      (nth (random (length keys)) keys))))

(defun generate-sequence (&key (max-len 25) (init-state (select-random-state)))
  "Generate a list of states, starting from the given one (if given) or from a random one if not given."
  (if (not (gethash init-state +model+))
      (error 'not-a-valid-state :nvstate init-state))
  (let ((seq (list init-state)))
    (dotimes (x max-len)
      (let ((next-state (select-next-state (first seq))))
	(if (null next-state)
	    (return-from generate-sequence (reverse seq))
	    (push next-state seq))))
    (reverse seq)))

