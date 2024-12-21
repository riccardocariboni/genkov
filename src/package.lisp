;;;; package.lisp

(defpackage #:genkov-utils
  (:use #:cl)
  (:export
   :punctuation-char-p
   :split-string-with-mark))

(defpackage #:genkov
  (:use #:cl)
  (:export
   ;; LOGIC
   ;; model
   :generate-model
   :reset-model
   :get-model
   :print-model
   ;; states
   :set-states
   :add-states
   :reset-states
   :get-states
   ;; GENERATION
   :generate-sequence))
