;;;; package.lisp

(defpackage #:advent-of-code-2023.util
  (:nicknames #:aoc2023.util #:util)
  (:use #:cl)
  (:export
   :transpose
   :histogram
   :split-lines
   :square
   :^2))

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



