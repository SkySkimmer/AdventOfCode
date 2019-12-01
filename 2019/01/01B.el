;; -*- lexical-binding: t; -*-

;; input should be in input.txt

(require 'seq)

(defun fuel-for (acc new)
  "Add fuel required for a mass NEW to ACC and return the result.
  This includes the fuel for the fuel for the mass etc
  recursively."

  (let ((newfuel (- (/ new 3) 2)))
    (if (> newfuel 0)
        (fuel-for (+ acc newfuel) newfuel)
      acc)))

;; unlike 01A.el, fuel-for protects against empty lines so no need for
;; OMIT-NULLS in split-string
(with-temp-buffer
  (insert-file-contents "input.txt")
  (let* ((data (mapcar #'string-to-number (split-string (buffer-string) "\n"))))
    (seq-reduce #'fuel-for data 0)))
