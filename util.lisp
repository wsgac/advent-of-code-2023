(in-package #:advent-of-code-2023.util)

(defun transpose (l)
  (apply #'mapcar #'list l))
