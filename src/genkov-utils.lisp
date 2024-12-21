;;;; genkov-utils.lisp

(in-package #:genkov-utils)

(defun punctuation-char-p (char)
  "Returns t if CHAR is a punctuation character."
  (find char "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"))

(defun split-string-with-mark (string)
  "Split STRING into words and punctuation, preserving punctuation marks as separate tokens."
  (let ((result '())
        (current-word '()))
    (loop :for i :from 0 :below (length string)
          :for char = (aref string i)
          :do (cond
		((or (alphanumericp char))
                 (push char current-word))
		((punctuation-char-p char)
                 (when current-word
                   (push (coerce (nreverse current-word) 'string) result))
                 (push (string char) result)
                 (setf current-word '()))
		((char= char #\Space)
                 (when current-word
                   (push (coerce (nreverse current-word) 'string) result))
                 (setf current-word '()))))
    (when current-word
      (push (coerce (nreverse current-word) 'string) result))

    (nreverse result)))
