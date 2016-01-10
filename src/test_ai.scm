;;;
;;; pacman-ai-test
;;;
;;; by Jan Cornelis
;;;
;;; Unit tests the a*-search algorithm
;;;
;;; Please note that I left off the version numbers in the planet expresssions
;;; When I try to run this program with the version numbers DrScheme complains
;;; ____________________________________________________________________________

(require "pacman-util.ss")
(require "pacman-file.ss")
(require "pacman-ai.ss")
(require (planet "test.ss" ("schematics" "schemeunit.plt")))
(require (planet "text-ui.ss" ("schematics" "schemeunit.plt")))
(require (planet "graphical-ui.ss" ("schematics" "schemeunit.plt")))

(define maze (file->maze "game-board.txt"))
(define graph (maze 'to-graph))

(define source (make-position 14 18))
(define target (make-position 21 20))
(define positions
  (list (make-position 15 18) (make-position 16 18) (make-position 17 18) (make-position 18 18) 
        (make-position 19 18) (make-position 20 18) (make-position 20 19) (make-position 20 20)
        (make-position 21 20)))

(define test-a*-search
  (let* ((maze (file->maze "game-board.txt"))
         (graph (maze 'to-graph))
         (source (make-position 14 18))
         (target (make-position 21 20))
         (positions
           (list (make-position 15 18) 
                 (make-position 16 18) 
                 (make-position 17 18) 
                 (make-position 18 18)
                 (make-position 19 18) 
                 (make-position 20 18) 
                 (make-position 20 19) 
                 (make-position 20 20)
                 (make-position 21 20))))
    (test-suite "Test of the A* Search"
                (test-equal? 
                 "Testing a search between two positions where there is only one shortest path"
                 (a*-search graph source target)
                 positions))))

(test/graphical-ui test-a*-search)
     
            