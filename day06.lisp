(in-package #:advent-of-code-2023.day06)


(defun parse-data (data)
  (apply #'mapcar #'cons
	  (mapcar (alexandria:compose (alexandria:curry #'mapcar #'parse-integer)
			       (alexandria:curry #'ppcre:all-matches-as-strings "[0-9]+"))
	   (split-lines data))))

(defun find-winning-time-range (data)
  (destructuring-bind (time . distance) data
    (let* ((delta (- (^2 time) (* 4 distance)))
	   (lower (1+ (floor (/ (- time (sqrt delta)) 2))))
	   (upper (1- (ceiling (/ (+ time (sqrt delta)) 2)))))
      (values ;; (list lower upper)
	      (+ upper (- lower) 1)))))

(defun puzzle-1 (&key (input *example-input-1*))
  (reduce #'* (mapcar #'find-winning-time-range (parse-data input))))

(defun puzzle-2 (&key (input *example-input-2*))
)

;;;; Data

(defvar *example-input-1*
  "Time:      7  15   30
Distance:  9  40  200")

(defvar *input-1*
  "Time:        54     94     65     92
Distance:   302   1476   1029   1404")

(defvar *example-input-2*
  *example-input-1*)

(defvar *input-2* *input-1*)
