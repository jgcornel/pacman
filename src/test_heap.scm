;;;
;;; heap-test
;;;
;;; by Jan Cornelis
;;;
;;; Contains a number of unit tests for pacman-heap
;;; They assure that the methods 'add!, 'remove!, 'pop! and 'top work as
;;; expected.
;;;
;;; Please note that I left off the version numbers in the planet expresssions
;;; When I try to run this program with the version numbers DrScheme complains
;;; ____________________________________________________________________________

(require "pacman-heap.ss")
(require (planet "test.ss" ("schematics" "schemeunit.plt")))
(require (planet "text-ui.ss" ("schematics" "schemeunit.plt")))
(require (planet "graphical-ui.ss" ("schematics" "schemeunit.plt")))

(define test-heap
  (test-suite "A unit test for the heap ADT"
              (test-eq?
               "Check 1"
               (let ((heap (make-heap 5 <)))
                 (heap 'add! 3) (heap 'add! 1) (heap 'add! 2) (heap 'add! 4) (heap 'top))
               1)
              (test-eq?
               "Check 2"
               (let ((heap (make-heap 5 <)))
                 (heap 'add! 3) (heap 'add! 1) (heap 'add! 2) (heap 'add! 4) (heap 'pop!) (heap 'top))
               2)
              (test-eq?
               "Check 3"
               (let ((heap (make-heap 5 <)))
                 (heap 'add! 3) (heap 'add! 1) (heap 'add! 2) (heap 'add! 4) (heap 'remove! 2) (heap 'pop!) (heap 'top))
               3)))
                 
(test/graphical-ui test-heap)
