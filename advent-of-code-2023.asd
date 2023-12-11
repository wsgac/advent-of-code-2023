;;;; advent-of-code-2023.asd

(asdf:defsystem #:advent-of-code-2023
  :description "Describe advent-of-code-2023 here"
  :author "Wojciech S. Gac <wojciech.s.gac@gmail.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:cl-ppcre :trivia :alexandria :array-operations :lisp-stat)
  :components ((:file "package")
	       (:file "util")
	       (:file "day01")
	       (:file "day02")
	       (:file "day03")
	       (:file "day04")
	       (:file "day05")
	       (:file "day06")
	       (:file "day07")
	       (:file "day08")
	       (:file "day09")
	       (:file "day10")
	       (:file "day11")))
