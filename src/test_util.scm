;;;
;;; pacman-util-test
;;;
;;; by Jan Cornelis
;;;
;;; Contains a number of unit tests for pacman-util
;;;
;;; Please note that I left off the version numbers in the planet expresssions
;;; When I try to run this program with the version numbers DrScheme complains
;;; ____________________________________________________________________________

(require "pacman-util.ss")

(require (planet "test.ss" ("schematics" "schemeunit.plt")))
(require (planet "text-ui.ss" ("schematics" "schemeunit.plt")))
(require (planet "graphical-ui.ss" ("schematics" "schemeunit.plt")))

(define test-position
  (let ((pos1 (make-position 1 5))
        (pos2 (make-position 3 4)))
    (test-suite "Tests for the position ADT"
                (test-eq? "(position-x pos1) should be 1" 
                          (position-x pos1) 1)
                (test-eq? "(position-y pos2) should be 5" 
                          (position-y pos1) 5)
                (test-true "(position=? pos1 pos1) should be true" 
                            (position=? pos1 pos1))
                (test-false "(position=? pos1 pos2) should be false"
                             (position=? pos1 pos2))
                (test-eq? "(position-manhattan-distance pos1 pos2) should be 3"
                          (position-manhattan-distance pos1 pos2) 3))))

(define test-direction
  (let ((dir1 (make-direction 0 1))
        (dir2 (make-direction 1 0))
        (pos1 (make-position 2 2))
        (pos2 (make-position 2 3))
        (pos3 (make-position 3 2)))
    (test-suite "Tests for the direction ADT"
                (test-eq? "(direction-delta-x dir1) should be 0"
                          (direction-delta-x dir1) 0)
                (test-eq? "(direction-delta-y dir1) should be 1"
                          (direction-delta-y dir1) 1)
                (test-true "(direction=? dir1 south) should be true"
                           (direction=? dir1 south))
                (test-true "(direction=? dir2 east) should be true"
                           (direction=? dir2 east))
                (test-false "(direction=? dir1 dir2) should be false"
                            (direction=? dir1 dir2))
                (test-true "(position-in-direction pos1 dir1) should be position=? to pos2"
                          (position=? (position-in-direction pos1 dir1) pos2))
                (test-true "(position-in-direction pos1 dir2) should be position=? to pos3"
                          (position=? (position-in-direction pos1 dir2) pos3)))))

(define test-matrix
  (let ((matrix (make-matrix 4 5))
        (pos1 (make-position 1 2))
        (pos2 (make-position 0 5)))
    (matrix-set! matrix 2 1 'test)
    (test-suite "Tests for the matrix ADT"
                (test-eq? "(matrix-number-of-rows matrix) should be 4"
                          (matrix-number-of-rows matrix) 4)
                (test-eq? "(matrix-number-of-cols matrix) should be 5"
                          (matrix-number-of-cols matrix) 5)
                (test-eq? "(matrix-ref matrix 2 1) should be equal to (matrix-position-ref pos1)"
                          (matrix-ref matrix 2 1) (matrix-position-ref matrix pos1))
                (test-true "(matrix-contains-position? matrix pos1) should be true"
                           (matrix-contains-position? matrix pos1))
                (test-false "(matrix-contains-position? matrix pos2) should be false"
                            (matrix-contains-position? matrix pos2)))))

(define test-varia
  (let* ((pos1 (make-position 0 1))
         (func (make-round-robin 1 2 3)))
    (test-suite "Tests for the general purpose functions"
                (test-equal? "(direction-from-to pos1 (position-in-direction pos1 south) should be south"
                             (direction-from-to pos1 (position-in-direction pos1 south)) south)
                (test-equal? "The 4th call of func should return 1"
                             (begin (func) (func) (func) (func)) 1))))                       

(test/graphical-ui test-position)
(test/graphical-ui test-direction)
(test/graphical-ui test-matrix)
(test/graphical-ui test-varia)
