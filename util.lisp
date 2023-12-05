(in-package #:advent-of-code-2023.util)

(defun transpose (l)
  (apply #'mapcar #'list l))

(defun histogram (list)
  (loop
    with h = (make-hash-table)
    for el in list
    do (incf (gethash el h 0))
    finally (return h)))
