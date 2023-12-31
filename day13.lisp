(in-package #:advent-of-code-2023.day13)

(defun parse-data (data)
  (mapcar (alexandria:compose (alexandria:curry #'mapcar (alexandria:rcurry #'coerce 'list))
			      #'split-lines)
	  (ppcre:split "\\n\\n" data)))

(defun has-horizontal-symmetry (pattern split-row &key (smudges 0))
  "Analyze horizontal symmetry of PATTERN about the horizontal line
before SPLIT-ROW. Require precisely SMUDGES discrepancies between the
two halves."
  (labels ((diffs (s1 s2)
	     (count nil (mapcar #'eq s1 s2))))
   (let ((upper (reverse (subseq pattern 0 split-row)))
	 (lower (subseq pattern split-row)))
     (= (reduce #'+ (mapcar #'diffs upper lower)) smudges))))

(defun find-horizontal-split-row (pattern &key (smudges 0))
  "Find if PATTERN has any horizontal symmetries, given the requirement
for SMUDGES discrepancies."
  (loop
    for row from 1 below (length pattern)
    if (has-horizontal-symmetry pattern row :smudges smudges)
      return row))

(defun puzzle-1 (&key (input *example-input-1*))
  (loop
    for l from 0
    for pattern in (parse-data input)
    for hsr = (find-horizontal-split-row pattern)
    for vsr = (find-horizontal-split-row (transpose pattern))
    if hsr
      sum (* 100 hsr)
    if vsr
      sum vsr))

(defun puzzle-2 (&key (input *example-input-2*))
    (loop
    for l from 0
    for pattern in (parse-data input)
    for hsr = (find-horizontal-split-row pattern :smudges 1)
    for vsr = (find-horizontal-split-row (transpose pattern) :smudges 1)
    if hsr
      sum (* 100 hsr)
    if vsr
      sum vsr))

;;;; Data

(defvar *example-input-1*
  "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")

(defvar *input-1*
  "
....####.#####...
..##..###.##.##.#
..##..###.##.##.#
....####.#####...
.....#.####.#.#.#
###.#####.####...
.#...#..##.#..###
##.....#.#.###...
..##.##..#..###..
####.#.#...#.##..
##.###..#.#....##
...##...#..##..#.
..#.####.##......
##.######....####
....#.#..#####...

..##..#.#.#..##
.#..#..#.#...#.
.#..#.#.#...#.#
.#..#.#.#...#.#
.#..#..#.#...#.
..##..#.#.#..##
#....###..#.#.#
.#.##........##
######.###.##..
#.##.#..#.##.#.
......##.#...##

......#
###.#..
###.##.
###.##.
###.#..
.....##
##..#..
##.#...
.###.#.
##.....
..#...#
#....##
#....##
..#...#
##.....

######...###.
##########.##
..##....#####
..##..###.#..
..##..#.#.#..
......#......
.#..#..#.#..#
..##..#...##.
#....#.#..##.
######.#####.
.......##..#.
.......##..#.
######.######

####.#.#......#
#.####.#..##.#.
###.###.###...#
####.#.##....##
.###..#.##..###
#....#....###.#
#...###..#.....
##..###..#.....
#....#....###.#
.###..#.##..###
####.#.##....##
###.###.###...#
#.####.#..##.#.
####.#.#......#
#...#.#.#..#.#.
#.##.##.#....##
#.##.##.#....##

....##.....##.###
....##.....##.###
.##.###.####....#
..#.....#.....#..
#..###.#.....#..#
#..##...#....##.#
#..#.#...####.#.#
#..##.#.#..#.##..
.##.......#######
.##.#...##.#..#..
.##.#...#.####...
....#.#.#.#.#..#.
.......#.#...####
....###..#.#...##
....#....#..#...#
.##..#.##...#.##.
######.##.#..#...

####.########.#
#####.#.##.#.##
##.#..........#
#####..####..##
....#.##..##.#.
....##.####.##.
#..##.#....#.##
.##...#....#...
#..#.#..##..#.#

##.##.####.##.#..
..#.#..#..##.#..#
.###...###.##.#..
.#..#.###.#....#.
###.##.#...##.##.
#####.##.###..###
#.##..#.#.#.#.#..
#.##..#.#.#.#.#..
#####.##.###..###
###.##.#...##.##.
.#.##.###.#....#.
.###...###.##.#..
..#.#..#..##.#..#
##.##.####.##.#..
##.##.####.##.#..

#####....
...#..#..
..#.#.#..
....#....
.......##
##.#...##
...#..#..
.######..
..#....##
..#.#####
...###...

.###..#..#.#..###
...#.###....####.
#...##......#.###
..#..#.######.#.#
#..#.#.....#...#.
####.###....##...
.#...###.#..#.#.#
#..#.#.#...##.###
#..#.#.#...##.###
.#...###.#..#.#.#
####.###....##...
#..#.#.....#...#.
..#..#.######.#.#
#...##......#.###
#..#.###....####.
.###..#..#.#..###
.###..#..#.#..###

#.#.##.#.####.#.#
#..#..#..####..#.
.##.##.##....##.#
#...##...#..#...#
.###..###.##.###.
..#....#.#..#.#..
..#....#......#..
####..##########.
...####........##
##.####.##..##.##
..#....#......#..
.#......#....#...
#.##..##.#..#.##.

...####..####..##
..##..#.#....#.#.
......#...##...#.
.###.##..#..#..##
#...###.#....#.##
#...###.#....#.##
.###.##..#..#..##
......#...##...#.
..##..#.#....#.#.
...####..####..##
######..#.......#
.#..##.#..##..#.#
..##..#..#..#..#.

.#.#.##....
#.#..#..##.
#.#..#..##.
.#.#.##....
#......##.#
.#..#.#.##.
.###.#.#..#
#.#########
###..#.....
##.##.#.##.
#.##...####
.##...##..#
#...###....
#..##.#....
#..#.#.####
.....#..##.
#..#.#.#..#

..#.#.#.#.#.#
#.##.###.#...
#..###..#...#
##...#..##.##
.#.#.#...#.##
.#..########.
..###.##....#
..###.##....#
.#..##.#####.
.#..##.#####.
..###.##....#
..###.##....#
.#..########.

#.###.#
##.#..#
#.####.
#####.#
...##..
....#..
#####.#
#..#..#
.##..##
#.#.#.#
#.#.#.#
.##..##
#..#..#
#####.#
....#..
...##..
#####.#

####...
.#...#.
##.####
###.###
###..##
.###...
.#.##..
....#..
#.#....
.##..##
.##..##
#.#....
....#..
.#.##..
.###...

#.#.#.#....
..##.##....
#.###.#..##
#.###.#..##
..##.##....
#.#.#.#....
...###.##.#
#..##...#..
.##.##..##.
.######.#..
#...#..####
#..#.#..##.
#..#.#..##.
#...#..##.#
.######.#..

##..##.#.....
.....##......
####.#.#.#..#
.#####.######
####.....####
.##.#####....
##.######....
..#.###......
#.##.#...####
#.....#.#....
#####........
.###..###....
.......#.....
##.##...#####
.......######

.####.###..##
#.##.#.#.##.#
.####........
#.##.####..##
#.##.###....#
#######..##..
.#..#........
..##..#..##..
......#..##..
#....##.####.
#.##.###.##.#
##..##.#....#
.####...#..#.
##..##.....#.
#....##.####.
.#..#.#.#..#.
##..##..#..#.

...##..######..##
..#..##......##..
...#.##.####.##.#
...##############
.#.##..######..##
.#.##..######..##
.#.##############
#.#..#........#..
.#..#..#....#..#.

#..####...####.
.#..#...###..##
....#..#.......
.#...#..###..##
###...#....##..
#.#####..######
..#..###.######
#.##.#.........
.##.##.##..##..
..#..#.#...##..
.#..#....######
#..#####...##..
.##..#..#..##..
#.#...#.#......
##.###..#......
..##..#####..##
.##....#.######

..##.#.##..#..##.
..##.#.##.#######
#.####.###.##.##.
#.####.###.##.##.
..##.#.##.#######
..##.#.##.##..##.
...##.#.##..#####
#.#.###.#..#.####
.###.##.#..#.#..#
..#.....#..#.####
##...#########..#
###.#.###..######
##.#..##...#.####
#..#.##...##.....
...##.#...#......

#...#.#..##
#...#.#..##
..##.######
..####.....
##.##.###.#
#.###.....#
#.###.#...#

....##.#..#..#..#
####.#######.####
#..######.####.##
.##...###.####.##
#..###..##.##.##.
.##....##.####.##
#####.#..........
#######....##....
#..#.....#....#..
.##.#..##......##
#####..##########

.#..##..#.#..
####..####.##
###....####..
#...##...##.#
.##.##.##...#
.##.##.##...#
#...##...##.#
###....####..
####..####.##
.#..##..#.#..
###.#..######
#...##...##.#
##......####.

#..####...##.
.#......#.##.
#..###.##.##.
#....###.####
####..###.##.
#..###...#..#
.#...##..#..#
.##.#.##.####
####...#.....
####...#.....
.##.#.##.####
.#...##..#..#
#..###...#..#
####..###.##.
#....###.####
#..###.##.##.
.#....#.#.##.

.#.#...........
..####..#######
....#####..##..
#...##..#.####.
#.###..........
.####.....####.
.####.#...####.

######.#.#.
#.##.##.#.#
#.##.##.#.#
######.#.#.
##..##.#...
.......#.#.
.....#.##..
######...#.
..##..##..#

..#####.#.###
###.#.#...#.#
###.#.#...#.#
..#####.#.###
###....#..###
#####..#...##
##.##.######.
##.....#..###
...##..##.##.
#..#.##.###..
##...#....##.

#.....#...#..####
.#..#...#.#...###
....##.##..#..###
#..#.#####..###..
#..#..###..##.###
.#.#.##.#...#..##
#...#.##..##.####
#...#.##..##.####
.#.#.##.#...#..##
#..#..###..##.###
#..#.#####..###..
....##.##..#..###
.#..#...#.#...###
#.....#...#..####
#####.#.#.####.#.

#.#.#.###
##.####.#
...###...
##...#...
###...##.
####..#..
####..#..

.######.#..
#.###....##
#...##..###
#...##.#...
##...###...
##...###...
##..##.#...

###.##.#....#
.#..####.##..
.#..####.##..
###.##.#....#
#.##..#.....#
..###..##..##
.#.#......#..
##.##..##..#.
.#..#..######
#...###.#.###
#...###.#.###
.#..##.######
##.##..##..#.

#..........
##..####..#
####.##.###
.#..####..#
##..#..#..#
..##....##.
.##########
#....##....
#..........
..##.##.##.
.#..#..#..#

..####..#
#..#.#..#
#.##.##.#
##..#.#.#
.#######.
#.##.#...
#.##.#...
.#######.
##..#.#.#
#.##.##.#
#..#.#..#
#.####..#
...#....#
...#....#
#.####..#
#..#.#..#
#.##.##.#

##..##.#.####..##
##..####.##.#..#.
######....#.#..#.
.......#...##..##
######..####.##.#
#....#.##########
.####.##.#.##..##
.......##.#.#..#.
#.##.####.#######
.#..#.#####..##..
.#..#.####..####.
#....##.##..####.
######......#..#.
####.###..#......
..##.......#.##.#
......#....#....#
........##..####.

...#.###...##
#..#.#.#.#..#
#..#.#.#.#..#
..##.###...##
#.####..#...#
#...####..###
...##...##.#.
...##...##.#.
#...####..###
#.####..#...#
..##.###...##
#..#.#.#.#..#
#..#.#.#.#..#
...#.###...##
#.......#.##.
#.##.#.#####.
###..########

....#.##.##.###..
.......###..#.##.
....#.##..#.#.###
....##.....#.#.##
####....###.#.#.#
......#..####....
.....###....#...#
######.#.#.#..#..
....#.#.##..#....
#####.###...#...#
#..##.###.##..##.

.#.#..#.#..
#..####..##
#..#..##.##
##.#..#.###
##......###
#.##..##.##
.#.####.#..

#..###...
.#...###.
.###.####
#..####..
##..#.#..
#.##.##..
.#..#....
#..##....
##..#####
#...#.###
###..#...
#..##..##
#..##..##
###..#...
#...#.###

.###.#..#.#.#
.###.#.##.#.#
..#..###...#.
#.#.#..#...#.
#....#.#.####
#....#.#.####
#.#.#..#...#.
..#..###...#.
.###.#.##.#.#

.###.#.
#.#..##
###....
....##.
#.##..#
#.##..#
..#.##.
###....
#.#..##
.###.#.
.###.#.

#..##..#..#...#
#.#..#.#..#....
###..###.......
#......##..#...
#.#..#.##..#.##
#.#..#.##..#.##
#......##..#...
###..###.......
#.#..#.#..#....
#..##..#..#...#
##....########.
#.####.###.#.##
##.##.##...##..
#.#..#.###.#...
..#####.####.##

######.#.#..#
####..#.#.#..
.##.#......#.
.....##......
.##..###.###.
.#####...###.
.##.###.##...
.....#..#.#.#
####...##.##.
.##.#...#..#.
####.#.......
####.#.....#.
.##.#.#....##
####.##.##.#.
####.##.##.#.

.##.##.##..
#.#....#.#.
.#......#.#
.###..###..
.##.##.##.#
.##.##.##.#
.###..###..
.#......#..
#.#....#.#.
.##.##.##..
.########.#
..######..#
.########.#

.#..#.#..#.#.
##..##....##.
#....######..
##..##....###
.#..#.#..#.#.
#.##.######.#
......####...

###..##
.....##
##..###
..#.###
###..##
..#..##
..#....
##.....
.#.#...

.####..##....
......#...###
..##..#...###
.......###...
#....####.###
.#####...#.##
#....####....
.........####
.#..#...#.#..
..##..#...#..
#....##..#.##
........#..##
.#..#.##.....
......#.###..
#....#..#.#..
##..##..#..##
######....###

####.###..###..##
.....###..###....
..##....##....##.
###.##.#..#.##.##
###...##..##...##
##.##..####..##.#
.....########....
...####....####..
...##..####..##..

#####.##.########
.#..#...##......#
..#.###.#........
..#.###.#........
.#.##...##......#
#####.##.########
.###.#.#...#..#..
.####.#.#.#.##.#.
.##.##.#.##....##

##...#.##..#.##
######.###.#.##
#.#.#..#..#####
#..#..#.##.....
...#.##.#..#.##
.#..##..#..####
#.#..#.##.#.#..
#...####.#..###
#...####.#..###
#.#..#.##.#.#..
.#..##..#..####
...#.##.#..#.##
#.....#.##.....
#.#.#..#..#####
######.###.#.##

...........#.##.#
##.##.##..##.#..#
###.#####..##.##.
........#.#.##..#
........#.#.##..#
###.#####..##.##.
##.##.##..##.#..#
...........#.##.#
............#..##
##....#####..#.#.
.#.##.#.##.#.....
##.##.##.###.###.
..#..#..###.#...#
.........#...####
...##...#.#..#.##

###....###.###...
..#.#..####.###.#
..#.#..####.###.#
###.#..###.###...
#.##...#.####....
#.#..#####....#.#
######..#.....#.#
#.#..#..##.#.....
#.#..#..##.#.....
######..#.....#.#
#.#..#####....#.#
#.##...#.####....
###.#..###.###...
..#.#..####.###.#
..#.#..####.###.#
###....###.###...
....#..##...##.##

###..###.#.##
#...##..##...
#.#.#...#..##
#.#.#...#..##
#...##..##...
###..###.####
..###...###..
#####....##..
#...#..##.###

.#.#...
.#.#...
..#..##
#.....#
###.#.#
#...#.#
..#.###
#.#.#.#
#.#.#.#
..#.###
#...#.#
#.#.#.#
#.....#

....#.##.
.....##..
#..#.##.#
#..#.##.#
.##.#..#.
#..#....#
#..##..##

##..##.#..###
.####....##..
#.##.####.##.
#.##.####.##.
.####....##..
##..##.#..###
..##.......#.
##..#########
#....###...#.
#######..#.#.
###.######...
..##....#..#.
#....####.#.#
#######.##...
.#..#..##..#.

##..#...#..#...#.
#.#..#..#..#..#..
.#.#...#.##.#...#
...#.##########.#
..###..##..#...##
#..#####.##.#####
.##.###.####.###.
#...#..#.##.#..#.
#...#..#.##.#..#.

...#.##
##..##.
##.##..
......#
######.
..#.#..
####...
###....
..###..
..#....
..#.#..

####..####..#..
..#.##.#..#.##.
..#.##.#..#....
.#......#..#..#
.##....##.#...#
....##........#
....##........#
.##....##.#...#
.#......#..##.#
..#.##.#..#....
..#.##.#..#.##.
####..####..#..
...####...#.###

##....#..#....#
...##.####.##..
..##..####..##.
..#.########.#.
##.#.#....#.#.#
..##..####..##.
...##......##..
###.#...#..#.##
..##.##..##.##.
###..#.##.#..##
..####....####.
.....#.##.#....
######.##.#####

#.#.#.#..#.#.
#.#.#.#..#.#.
#..#.#....#.#
..###.#..#.##
#.....####...
..##.#.##.#.#
.#.###.##.###
#.##.##..##.#
....#.####.#.
###.#......#.
.##.#.###..#.

###.##.##.##.
###.##.##.##.
####..#..#..#
.####..##..##
####........#
####.######.#
..#..#.##.#..
...##...#..##
#.###########
##.#..####..#
.....##..##..

####.##.###..#.
####.##.###..#.
.#...#...#.####
..#..#.#.#..##.
#.#.#....#.##.#
.######.#....##
#####.##.###...
..#...##.....#.
..#...##.....#.
#####.##.####..
.######.#....##

###....####
##.#..##..#
..#.##.#..#
....#..####
#...#......
..####.....
....##.....
..##.......
....####..#
...#.......
##.#....##.

....##.####
..#.....##.
..#.....##.
....##.####
##....##.##
..####.#.##
.#..####...
#.#####....
.#.#..#..##
##.#....###
.#.#....###
.#.#..#..##
#.#####....
.#..####...
..####.#.##
##....##.##
....##.####

##..##..#..
.......##.#
.......##.#
##..##..#..
.#..#.#####
..##.###...
.#..#..###.

##..##.#.#.
##..##.#.#.
..#......#.
.#.##.#..#.
#..#.#....#
##.##.#..#.
##.##.#.##.
#..#.#....#
.#.##.#..#.
..#......#.
##..##.#.#.

#.#######..######
..#..##......##..
#.###..##..##..##
##.##############
#..##############
###.####....####.
...##..##..##..##
#.###..######..##
.##.####.##.####.
#########..######
#.#.#..#.##.#..#.
....##.#....#.##.
.##.#..#....#..#.
##.#.##.#..#.##.#
..##....#..#....#

##.#.###..###.#..
#.#.#.#....#.#.#.
##.....#..#.....#
#.#.##.#..#.##.#.
###..#......#..##
...#..........#..
##...#......#...#
##.#.#.#..#.#.#.#
#..##.#....#.##..
.#..##.####.##..#
#..##........##..
##..#..####..#..#
##.##.##..##.##.#
#.#.###.##.###.#.
.#...#......#...#
#..###......###..
#..###......###..

###...#...#....
.##...#.#....##
##.##..#...##..
#.#.###.##..###
#####.#..####.#
#.##.##..######
#.##..#..######
#####.#..####.#
#.#.###.##..###
#.#.###.##..###
#####.#..####.#
#.##..#..######
#.##.##..######
#####.#..####.#
#.#.###.##..###

........#
..####...
#.####.##
#.#..#.##
##.##.###
.######..
.#....#..
.#....#..
..####...

####..#
..##..#
##..##.
#......
#.#.##.
..#....
##..##.
.##....
.#.####
##.....
#.#####
##.####
...#..#
#.##..#
#.#....
..#....
#.##..#

.#..#.#.##.####.#
#...##..#...##...
#...####.##....##
.##..#.###..##..#
#..#.##..##....##
.....##.#..#..#..
###......#......#
###......#......#
.....##.#..#..#..
#..#.##..##....##
.##..#.###..##..#
#...####.##....##
#....#..#...##...

##.##.###.##.
##.###...#..#
#..##.##.....
#..##.##.....
##.###...#..#
##.##.###.##.
##..##.#..##.
#.##..#...##.
.#...#.####.#

##.#....#
.#.##.#..
...##..##
#..#..#..
.###...##
.#.######
..#..##..
.#.#.####
.#.#.####
..#..##..
.#.######

###.#.#....#.
#.#.#..####..
....#.######.
...#..#.##.#.
.######.##.##
.##....#..#..
......##..#..
#..#.##....##
##....######.
..####......#
..####......#

......#.#
####.##..
#....##.#
######.#.
.####.##.
.####.##.
######.#.

##.#..#.#######
..##..##.......
###....####..##
.#.####.#......
..#.##.#.......
#..#..#..######
....##....#..#.
...#..#...####.
###.#..###....#

#......##......
....#..##..#...
.##############
.####......####
##..##....##..#
##..#......#..#
.######..######

###..##..##..##
#..#.#.##.#.#..
##..#######...#
##....####....#
##.#..####..#.#
######....#####
.##..........##
#.....#..#.....
####.##..##.###
####.##..##.###
#.....#..#.....
.##..........##
######....#####

..###.####.
##...######
.###.#....#
####.##..##
###..#.##.#
###.#.#..#.
....##.##.#
###....##..
###....##..
....##.##.#
###.#.#..#.
.##..#.##.#
####.##..##

###.#..####..#.##
..#####....#####.
...#....##....#..
.#..###....###..#
...#.#.####.#.#..
###.#..####..#.##
###.##.#..#.##.##

##.#..#####
..#..#.....
###..######
.######.##.
##....##..#
.#....#.##.
###..###..#
.##..##.##.
##....##..#
.######....
.######.##.
...##......
#..##..####

.##.#.#..#.#.
#..##.#..#.##
.##.##.##.##.
.##....##....
###.########.
#..####..####
######....###

.#.####..
#.#.##.#.
#..##.##.
...#...##
#..#.#...
#..#.#...
...#...##
#..##.##.
#.#.##.#.
.#.####..
...##....
.##.#...#
.##.#...#
...##....
.#.####..
#.#.##.#.
#...#.##.

....#.......####.
#####...#....##..
#..#.#..#..#.##.#
###....#.#.######
.##...#.###..##..
....#..##.##....#
.......##.#.####.
....#..##.#.####.
#..#.####.#.#..#.
.##.##..###.#..#.
####..###..######

##..#..##.#
##..#..##.#
##.###...##
###.#.#....
###.###..##
.#..#....#.
..#..#.....
#.##...#...
...#..#.##.
###..#.....
####.#.....

#.##.##.#####
.#.#.##.#.#..
#...#..#...##
...##..##....
###.#..#.####
..###..###...
...#.##.#....

........##..#
.##..##.#.##.
#.####.#.####
.#....#..####
#..##..#..##.
###..########
..#..#.......
#......#.##.#
.........####
.........####
.#....#..####

#..#..##.#.#.##
.##..##......##
.#...#.########
#..#..##..#...#
#..#..##..#...#
.#...#.########
.##..##......##
#..#..##.#.#.##
..####.##.##..#
##.#.####...#..
##.#.####...#..
..####.##.##..#
...#..##.#.#.##

..####...##.#.##.
#####.###..##....
#####.###..##....
..####...##.#.##.
##.....#..#.#...#
#####....#.#.##.#
......#.##.#...##
..##....#.....##.
##.#.##..#.#..###
...#.####....#..#
.###........##...
##.#.###.#..##..#
..##.###.##..####
###.######...##.#
##..###.####.....
###..##...####.##
..###...##.###.##

#....##..#..#.##.
##..#.##.####....
..##....#.####.##
.##....#.#.######
#......#....##..#
#..###...#.#.####
#.###.#.#.#.#.##.
##..#..#.##.#.##.
.#.###.#..####..#
.#.###.#..####..#
##..#..#.##.#.##.
#.###.#.#.#.#.##.
#..###...#.#.####
#......#....##..#
.##....#.#.######

.....#.#...
#..#..#.###
#####....##
#..####...#
....#.#####
..#.#.##..#
.##...#....
......#.#.#
......#.#.#

###..######
.#.##.#..#.
.#.##.#..#.
.#....#..#.
#.#..#.##.#
#..##..##..
.#....#..#.
#########.#
...##......

.........####.##.
.#..#...#.##.###.
.#..#.#..##.#####
.#####.......##.#
#.##.###..##.####
.####....#..##.#.
...............#.
######.#..###..#.
.####..#.#......#
.####..#.#......#
######.#..###..#.

..##..##.##
.####.#.#.#
.......##..
.......##..
.####.#.#.#
..##..##.##
#.##.#.#...
..##.##....
#.##.#.#.##

####..#..
.##..#.#.
####.###.
####.###.
.##....#.
####..#..
####.###.
#..###.##
#..#####.

...##..
#.#..#.
.##...#
.##...#
#.#..#.
...##..
...##..
#....#.
.##...#

###.#.#...#.###
..##.#####.###.
..##.#####.###.
###.#.....#.###
###..#.#...####
###....##..##..
###..#.###..##.
...#.###.##..#.
#######...##.##
##.#..#.#..#.##
###..#.#..#....")

(defvar *example-input-2*
 *example-input-1*)

(defvar *input-2* *input-1*)
