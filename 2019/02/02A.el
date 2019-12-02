;; -*- lexical-binding: t; -*-

;; input should be in input.txt

(defun run-from (data i)
  "Run program DATA starting at opcode I. Modifies DATA in-place."
  (let ((opcode (elt data i)))
    (cond
     ((= opcode 99) nil)
     ((= opcode 1)
      (setf (elt data (elt data (+ i 3)))
            (+ (elt data (elt data (+ i 1)))
               (elt data (elt data (+ i 2)))))
      (run-from data (+ i 4)))
     ((= opcode 2)
      (setf (elt data (elt data (+ i 3)))
            (* (elt data (elt data (+ i 1)))
               (elt data (elt data (+ i 2)))))
      (run-from data (+ i 4))))))

(with-temp-buffer
  (insert-file-contents "input.txt")
  (let* ((data (mapcar #'string-to-number (split-string (buffer-string) ","))))

    ;; reset 1202 alarm
    (setf (elt data 1) 12)
    (setf (elt data 2) 2)

    (run-from data 0)

    (car data)))
