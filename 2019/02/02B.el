;; -*- lexical-binding: t; -*-

;; input should be in input.txt

(require 'subr-x)

(defun safe-aref (array idx)
  (when (< idx (length array)) (aref array idx)))

(defun run-from (data i)
  "Run program DATA (a vector) starting at opcode I. Modifies DATA in-place.
On invalid programs, abort and put nil at the beginning of data."
  (let ((opcode (safe-aref data i)))
    (cond
     ((= opcode 99) nil)
     ((= opcode 1)
      (if (when-let* ((p1 (safe-aref data (+ i 1)))
                      (p2 (safe-aref data (+ i 2)))
                      (p3 (safe-aref data (+ i 3)))
                      (lop (safe-aref data p1))
                      (rop (safe-aref data p2)))
            (when (< p3 (length data))
              (aset data p3 (+ lop rop))))
          (run-from data (+ i 4))
        (aset data 0 nil)))
     ((= opcode 2)
      (if (when-let* ((p1 (safe-aref data (+ i 1)))
                      (p2 (safe-aref data (+ i 2)))
                      (p3 (safe-aref data (+ i 3)))
                      (lop (safe-aref data p1))
                      (rop (safe-aref data p2)))
            (when (< p3 (length data))
                (aset data p3 (* lop rop))))
          (run-from data (+ i 4))
        (aset data 0 nil)))
     (t (aset data 0 nil)))))

(defun try-vals (noun verb expected)
  "Test whether values NOUN and VERB produce EXPECTED. Return t
if so, nil otherwise."

  (with-temp-buffer
    (insert-file-contents "input.txt")
    (let* ((data (apply #'vector (mapcar #'string-to-number (split-string (buffer-string) ",")))))

      ;; reset 1202 alarm
      (aset data 1 noun)
      (aset data 2 verb)

      (run-from data 0)

      (equal (aref data 0) expected))))

;; sanity check
;;(try-vals 12 02 6627023)

;; warning! no tailrec optim in elisp
(defun find-input-from (noun verb expected)
  (while (and (< noun 100) (< verb 100) (not (try-vals noun verb expected)))
    (if (= verb 99)
        (setq noun (+ noun 1)
              verb 0)
      (setq verb (+ verb 1))))
  (if (not (and (< noun 100) (< verb 100))) nil
    (+ (* noun 100) verb)))

(find-input-from 0 0 19690720)
