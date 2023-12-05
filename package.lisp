;;;; package.lisp

(defpackage #:advent-of-code-2023.util
  (:nicknames #:aoc2023.util #:util)
  (:use #:cl)
  (:export :transpose
	   :histogram))

(defpackage #:advent-of-code-2023.day01
  (:nicknames #:aoc2023.day01)
  (:use #:cl #:util))

(defpackage #:advent-of-code-2023.day02
  (:nicknames #:aoc2023.day02)
  (:use #:cl #:util))

(defpackage #:advent-of-code-2023.day03
  (:nicknames #:aoc2023.day03)
  (:use #:cl #:util))

(defpackage #:advent-of-code-2023.day04
  (:nicknames #:aoc2023.day04)
  (:use #:cl #:util))



