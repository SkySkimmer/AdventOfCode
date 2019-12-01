;; -*- lexical-binding: t; -*-

;; input should be in input.txt

(require 'seq)

;; careful: if split-string called without OMIT-NULLS argument (the t
;; at the end), an empty line at the end of the file produces a 0 in
;; data then a -2 in required-per
(with-temp-buffer
  (insert-file-contents "input.txt")
  (let* ((data (mapcar #'string-to-number (split-string (buffer-string) "\n" t)))
        (required-per (mapcar (lambda (mass) (- (/ mass 3) 2)) data)))
    (seq-reduce #'+ required-per 0)))
