;;;
;;; file-test
;;;
;;; by Jan Cornelis
;;;
;;; Contains a unit tests for pacman-file
;;;
;;;
;;; Please note that I left off the version numbers in the planet expresssions
;;; When I try to run this program with the version numbers DrScheme complains
;;; ____________________________________________________________________________

(require "pacman-file.ss")

(require (planet "test.ss" ("schematics" "schemeunit.plt")))
(require (planet "text-ui.ss" ("schematics" "schemeunit.plt")))
(require (planet "graphical-ui.ss" ("schematics" "schemeunit.plt")))

(define test-file
  (test-suite "A unit test for file->maze"
              (test-true
               "Creating a Maze from game-board.txt should work"
               (procedure? (file->maze "./game-board.txt")))
              (test-false
               "Creating a Maze from scores.txt should not work"
               (file->maze "file-test.scm"))))
                 
(test/graphical-ui test-file)
