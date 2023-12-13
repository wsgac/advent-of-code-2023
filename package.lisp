;;;; package.lisp

(defpackage #:advent-of-code-2023.util
  (:nicknames #:aoc2023.util #:util)
  (:use #:cl)
  (:export
   :transpose
   :histogram
   :split-lines
   :square
   :^2
   :join-numbers
   :position-in-2d-array
   :parse-string-into-array
   :parse-string-into-list))

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

(defpackage #:advent-of-code-2023.day05
  (:nicknames #:aoc2023.day05)
  (:use #:cl #:util))

(defpackage #:advent-of-code-2023.day06
  (:nicknames #:aoc2023.day06)
  (:use #:cl #:util))

(defpackage #:advent-of-code-2023.day07
  (:nicknames #:aoc2023.day07)
  (:use #:cl #:util))

(defpackage #:advent-of-code-2023.day08
  (:nicknames #:aoc2023.day08)
  (:use #:cl #:util))

(defpackage #:advent-of-code-2023.day09
  (:nicknames #:aoc2023.day09)
  (:use #:cl #:util))

(defpackage #:advent-of-code-2023.day10
  (:nicknames #:aoc2023.day10)
  (:use #:cl #:util))

(defpackage #:advent-of-code-2023.day11
  (:nicknames #:aoc2023.day11)
  (:use #:cl #:util))

(defpackage #:advent-of-code-2023.day12
  (:nicknames #:aoc2023.day12)
  (:use #:cl #:util))

(defpackage #:advent-of-code-2023.day13
  (:nicknames #:aoc2023.day13)
  (:use #:cl #:util))
