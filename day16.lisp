(in-package #:advent-of-code-2023.day16)

;; Represent bearing as a unimodular complex number
(defconstant +i+ #c (0 1))

(defun propagate-beam (beam array visited)
  (let* ((bearing (getf beam :bearing))
	 (new-row (+ (getf beam :row) (imagpart bearing)))
	 (new-col (+ (getf beam :column) (realpart bearing))))
    (setf (getf beam :row) new-row)
    (setf (getf beam :column) new-col)
    (when (and (ignore-errors (aref array new-row new-col))
	       (not (gethash beam visited)))
      (setf (gethash beam visited) t)
       beam)))

(defun handle-beam (beam array)
  (let* ((bearing (getf beam :bearing))
	 (row (getf beam :row))
	 (col (getf beam :column))
	 (field (aref array row col)))
   (case field
     (#\. (list beam))
     (#\\ (let ((rot (if (zerop (imagpart bearing)) (- +i+) +i+)))
	    (setf (getf beam :bearing) (* bearing rot))
	    (list beam)))
     (#\/ (let ((rot (if (zerop (imagpart bearing)) +i+ (- +i+))))
	    (setf (getf beam :bearing) (* bearing rot))
	    (list beam)))
     (#\- (case (imagpart bearing)
	    ;; h
	    (0
	     (list beam))
	    ;; v
	    (1
	     (let ((h1 (copy-list beam))
		   (h2 (copy-list beam)))
	       (setf (getf h1 :bearing) (* bearing +i+))
	       (setf (getf h2 :bearing) (* bearing (- +i+)))
	       (list h1 h2)))))
     (#\| (case (imagpart bearing)
	    ;; h
	    (0
	     (let ((v1 (copy-list beam))
		   (v2 (copy-list beam)))
	       (setf (getf v1 :bearing) (* bearing +i+))
	       (setf (getf v2 :bearing) (* bearing (- +i+)))
	       (list v1 v2)))
	    ;; v
	    (1
	     (list beam)))))))

(defun puzzle-1 (&key (input *example-input-1*))
  (let ((init '(:row 0 :column 0 :bearing 1)))
   (loop
     with array = (parse-string-into-array input)
     with visited = (make-hash-table :test #'equal)
     with beams = (list init)
     initially (setf (gethash init visited) t)
     while beams
     do (print beams)
     do (setf beams
	      (remove nil
	       (mapcar (alexandria:rcurry #'propagage-beam array visited)
		       (mapcan (alexandria:rcurry #'handle-beam array) beams))))
     finally (return visited))))

(defun puzzle-2 (&key (input *example-input-2*))
  )

;;;; Data

(defvar *example-input-1*
  ".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....")

(defvar *input-1*
  "\\..-.-..../.....|......\\......................\\............\\.................|..............|........./.|.....
...-\\...|............|......../.|........-../................|.-..............-....\\.|..|.....................
.|..........\\................\\\\|................../..........................\\\\../|...-...-.|..........\\......
..................|\\....|.....-.......................-........\\.............................|/...............
.......-...........-......................./........../.......-............................\\.....-.....-.../..
./.\\...../....../..........\\..............|............|....|........................\\..../.../...|\\..........
..................................../...............................................\\..................-.\\....
......../.........................|...............................|...............\\|\\........\\....|....\\......
.--.........................../.../.......-.\\........../.\\.........|...............\\..|.....\\-.....\\....||\\...
......\\............|......../..\\................-..............\\-..-...|\\.../-............/.........|...\\.....
.........|.........-.........|............/............-..............\\../.......-.|\\\\..|......./.............
..........................\\........|.....\\...../..-......-............../.-|.............\\.\\...../.......\\....
.....\\.................-........../........-....|.....-......|.....................................-......-\\..
......|\\......................................./...../........|...|....../..................-....-.-..../.....
.../......\\...|./|..........\\.......-./..-../.................\\.....\\......../........./..|......-...|.../....
--|................\\.................|........../......\\|......|.........................\\....................
......../...........|./............../.....-.....-......../.................................-.....-...........
........................-.../..../..\\|..............................|\\..|.|\\................/........-...\\...|
\\.........-....../...|.\\............./......\\.......\\..../....\\|./-|.........|......-.......\\............|....
....|..........-.....|..../..........-/...-.............\\..\\...................\\................|......|......
............-..|...........-..../...-........../.../...-....-....../\\..............|........\\..............--.
.....\\.../......|..-..............|..-........................|......|.......|.........\\............/...\\...-.
|....................-.........../.....-........\\.../..........|..............................................
..-.............|..../............|........................|..../.........................\\..........-..-.../.
........-.|......\\.........../................/..|..\\................./.|....-................../...-......./.
\\|..-.......\\..............\\.|/......-|.........../........../.-...............\\....\\....\\..|..|..............
..........|.........\\............\\\\.../../|...........-................|....................|....|-\\/\\...\\....
.-........\\............-.......\\............-.............\\.\\......./...........././.|.\\-...-.\\./..........-/.
../.................-........//--...........|........./..../....|.....................................|....\\..
.............|....|........|........./.....\\..../..-..................../........|.....././...................
......................-....\\..........--.....\\..\\........../...........................--/....................
.|.../.......-.......\\.......|.............................-.............\\..-.\\../.......|............|......\\
...../.......-......|.............../.../...........................................|......-..........-.......
.............\\............................................../......................\\......../.................
|....................-...-\\..................\\................/.../...........................................
..|....-............/....................../......../...../......./.......\\.-................/................
.........../..-/...-...\\..............-..../...............-|.........-............/........|.|.-...|...\\.....
....../..-../.........-|...../...........................\\..............-..................--...-.............
-.................-......../.....|....................\\....................-../....................|./.....-..
......./...\\......../.|.|.........-.................................../....|..................................
...|........\\|\\.........\\...\\...-......-...............................\\.......//.|.....-.............../.....
........................../.|-...|....\\..........\\...................-..-............../............../.......
........../..-.......-....\\.......\\.............\\.|................................|......../..............-..
.......-................................-..................|../...\\....................................-......
...................................................................................../...\\...................-
..\\.-...................................|..............-.....|.........\\..................../.........-....\\..
...................../\\......-...........--.../............|...........|.....\\....//.....-........./...-|.....
......||.........|.-.........................|.........|.||..-....-......|.................\\.-../.............
...|.........|.......\\....\\................................../.................../....\\................\\......
..................../|...............||.-/........|../..................................../.\\......\\...-......
......./........./..-............................-.................\\...|...............-................-.....
.............................--...-....\\......................................|..........\\...-......./........
.......\\...-.............../......\\/.........................|........./..........|...........................
.........|.................................|.........../.|................................../...........-....|
...|...-...|...........................-....-.....................|../|........|...................../.....|..
.......-.....-..................\\.-........|...-..../........\\.......................\\......|..........-......
|......................\\..........-...............|........................../...........|....................
................|....|..................-......-/.......-....|..\\.../\\.|.....-..../...................../.|..\\
............/........................../....-........-...............-...|...............|..\\............./...
................./................\\.............../...........\\........\\....../............\\.....|........./..
..\\......./.-.....................-............-./......|..........-.............\\..../.......................
/..../....-........\\................................\\....|.............../\\-.............................-|...
................\\..................\\.--.|......//....|..|....|....../..................|.|...|....-|-.........
...................\\............\\....-............\\...........................................-./.............
.........................\\../............|..\\.........../................/..../.............-.................
..-..\\.........../............|..................................../............/.....-..............|.....|..
..|/......\\||.......\\............/....|........|.-........./.............|.....................|./............
..........-............../...........................................|.............|..-..../.|................
..|.../..|......................./..-.-..............\\../....................-..|............-...../.-........
.-....../..............\\..................\\................./....-...\\....\\...-.....\\.....-.-..............\\./
............./..../.\\........-..\\................................\\....../.|.....|.......|.........|....\\......
.-.....\\.-......-.|.-.........../..|...........\\....................................\\........|.//./.......-...
......-.......-/.........................-...../........|............./..................../............-.....
.............\\....|......-../...........\\...................../...............................................
../.......................................-...../.|..\\......|...-......../....|./..........\\.........|........
....../.............\\...................................\\........\\....-.........../..../..\\..................|
.../...-.....-................/.........|....................|..............-.........\\.......................
...................../..............-.......|......................./...|.....|../.|............../..........\\
.././....../........-........|.../.........../|..........|....../......-.....\\.............................\\..
............|........|................/....\\..............\\............/.....\\../..............|....../.......
|................\\..../...........................|.......\\......\\.....\\........../....../....................
........./...-...\\......./....................-...................................-............\\........-..../
............./............../.....................\\.-..../.................\\.........|.|............../.-...\\\\
.........|......-\\......\\.................../..|.........|......\\....\\..-..\\-.................................
...............|../.......................-.../......\\......|||................|..............................
.|...-............./...........-.......................|...............................|..............\\.......
.|.............|.........................................-........|-.\\.................-/.|...................
.|.......-||.........../..\\....\\.....................-....-....-.......-......-.........-|........|...........
.........\\........-...|........../............/.................../......./........................-..........
............\\.........................../..\\.......-............/.......................|.........\\...........
..\\..\\............/..................../..............-....../|.../...................|............./..../....
..\\.\\....\\.../............/.../.........................../.-.......-.....\\......|........|...../.........|.\\|
.....-...........\\..............-......-....-...../........-......|...../..\\..|/........-............./.../...
.....\\.......-........\\...-.............................................|....||.........|...-......\\..........
............|......................../.....-........././..............................................\\.....\\.
.\\.........\\......../........................-.............|........\\../.../...-......\\................|.....-
....|................|/.....................\\........|........................./|.....\\...-../.|......../-....
/..............-......-..............|..|.|.....................-\\.|../...\\..............................|.../
....\\/.........|\\....-..............\\......-.....................\\............\\........................\\......
......-...................\\...............-/../........-.../....................\\./..........-................
..........................\\.......|......\\.|\\....../......\\......../........-/............................/.|.
.......-....\\...................................../..............|../.......|......./..........|--.......-....
...-...|.............................|........-.....-.|...........|.....|..............\\..............-.......
-.\\...........\\......|.\\.............................\\...........|.........\\.............../\\.................
...../....................\\../|...........................\\.........|..................|........\\.|..-../.....
.-....-../.................................\\....../......|....../..............-...........-../...\\.../.......
.\\........-............................../..-...................|/..........|............................/....
../|..........|.........../...-....../.....|.-./...\\....-....\\|.\\.....|........./.-.....\\.....|./........|....
............\\.....\\...................-.\\-.-\\|...............................\\...............\\.....-..........
...|................-......................\\....|..\\.-..........|............../.....-...................../..")

(defvar *example-input-2*
  *example-input-1*)

(defvar *input-2* *input-1*)