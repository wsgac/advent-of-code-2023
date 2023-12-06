(in-package #:advent-of-code-2023.day06)


(defun parse-data (data)
  "Parse DATA into a list of (time . distance) cons cells."
  (apply #'mapcar #'cons
	 (mapcar (alexandria:compose (alexandria:curry #'mapcar #'parse-integer)
				     (alexandria:curry #'ppcre:all-matches-as-strings "[0-9]+"))
		 (split-lines data))))

(defun find-winning-time-range (data)
  "Compute the range of winning charge times by solving the quadratic
inequality t_c*(t-t_c) > d, where t_c is charging time and d is the
record distance."
  (destructuring-bind (time . distance) data
    (let* ((delta (- (^2 time) (* 4 distance)))
	   (lower (1+ (floor (/ (- time (sqrt delta)) 2))))
	   (upper (1- (ceiling (/ (+ time (sqrt delta)) 2)))))
      (values (+ upper (- lower) 1)
	      ;; (list lower upper)
	      ))))

(defun puzzle-1 (&key (input *example-input-1*))
  (reduce #'* (mapcar #'find-winning-time-range (parse-data input))))

(defun stick-data (data)
  "Reassemble DATA into a single time-distance pair, as if ignoring
spaces in the original input."
  (reduce #'(lambda (a b)
	      (cons (join-numbers (car a) (car b))
		    (join-numbers (cdr a) (cdr b)))) data))

(defun puzzle-2 (&key (input *example-input-2*))
  ;; The quadratic equation approach gave me a result off by 2, but
  ;; only with the data proper. Need to investigate the root
  ;; computations (find-winning-time-range (stick-data (parse-data
  ;; input)))
  (destructuring-bind (time . distance)
      (stick-data (parse-data input))
    (loop
      for tt from 1 below time
      count (> (* tt (- time tt)) distance))))

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
