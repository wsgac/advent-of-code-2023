(in-package #:advent-of-code-2023.util)

(defun transpose (l)
  (apply #'mapcar #'list l))

(defun histogram (list)
  (loop
    with h = (make-hash-table)
    for el in list
    do (incf (gethash el h 0))
    finally (return h)))

(defun split-lines (string)
  (uiop:split-string string :separator '(#\Newline)))

(defun square (x)
  (* x x))
(setf (fdefinition '^2) #'square)

(defun join-numbers (a b)
  (+ (* a (expt 10 (ceiling (log b 10)))) b))
