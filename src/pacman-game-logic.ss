;;;
;;; pacman-game-logic.ss
;;;
;;; Jan Cornelis
;;; 2007-2008
;;;
;;; This module implements a dispatch table of procedures
;;; it is used by the pacman-game-loop.ss module when determining the 
;;; impact of Agents bumping into each other and Agents encountering
;;; Rewards. Calling game-logic-lookup with the types of two objects
;;; will return a procedure with the following signature:
;;;
;;;  procedure : object object Event-Queue number -> void
;;; 
;;; The number represents the level in which the procedure should be
;;; executed. For now the level is not used in the procedures provided.
;;;
;;; When nothing is found, #f is returned
;;; Before calling game-logic-lookup, one should call game-logic-install
;;;
;;;________________________________________________________________________

(module pacman-game-logic mzscheme
  
  (require "pacman-util.ss"
           "pacman-event.ss"
           "pacman-glob.ss")
  
  (provide game-logic-install ; -> void
           game-logic-lookup) ; symbol symbol -> procedure
  
  ;;; Module global variables
  ;;;______________________________________________________________________
  
  (define *lookup-table* (make-table))
  
  (define *ghost-to-normal-events* #f)
  
  (define *killed-ghost-index* 0)
  
  ;;; Interface functions
  ;;;______________________________________________________________________
  
  ;;; game-logic-install : -> void
  ;;;______________________________________________________________________
  (define (game-logic-install)
    (set! *ghost-to-normal-events* #f)
    (table-insert! *lookup-table* 'pacman 'ghost     pacman-ghost)
    (table-insert! *lookup-table* 'pacman 'cookie    pacman-reward)
    (table-insert! *lookup-table* 'pacman 'fruit     pacman-reward)
    (table-insert! *lookup-table* 'pacman 'energizer pacman-energizer)
    (table-insert! *lookup-table* 'ghost 'ghost-home ghost-ghost-home))
  
  ;;; game-logic-lookup : symbol symbol -> procedure
  ;;;______________________________________________________________________
  (define (game-logic-lookup symbol1 symbol2)
    (table-lookup *lookup-table* symbol1 symbol2))
  
  ;;; Functions to be inserted into *lookup-table*
  ;;;______________________________________________________________________
  
  ;;; pacman-ghost : Pacman Ghost Event-Queue number -> void
  ;;;______________________________________________________________________
  (define (pacman-ghost pacman ghost event-queue level)
    (let ((ghost-state (ghost 'get-state)))
      (cond
        ((eq? ghost-state 'strong)
         (pacman 'decr-lives)
         (if (= (pacman 'get-lives) 0)
             (pacman 'set-state! 'dead)
             (pacman 'set-state! 'weak)))
        ;;
        ;; When a Ghost is killed the number of points will depend upon
        ;; how many Ghosts were already killed since Pacman ate an 
        ;; Energizer
        ;;
        ((eq? ghost-state 'weak)
         (set! *killed-ghost-index* (+ *killed-ghost-index* 1))
         (pacman 'add-points (* *killed-ghost-index* DEAD-GHOST-SCORE))
         (ghost 'set-state! 'dead)))))
  
  ;;; pacman-reward : Pacman Reward Event-Queue number -> void
  ;;;______________________________________________________________________
  (define (pacman-reward pacman reward event-queue level)
    (let* ((points (reward 'get-score))
           (maze (pacman 'get-maze))
           (pos  (pacman 'get-position))
           (cell (maze 'get pos)))
      (cell 'set-reward! #f)
      (pacman 'add-points points)))
  
  ;;; pacman-energizer : Pacman Energizer Event-Queue number -> void
  ;;;______________________________________________________________________
  (define (pacman-energizer pacman energizer event-queue level)
    (let* ((maze (pacman 'get-maze))
           (the-ghosts (maze 'get-the-ghosts)))
      (pacman-reward pacman energizer event-queue level)
      ;;
      ;; Reset the number of Ghosts killed since Pacman ate an Energizer
      ;;
      (set! *killed-ghost-index* 0)
      (if (not *ghost-to-normal-events*)
          (set! *ghost-to-normal-events*
                (map 
                 (lambda (g)
                   (make-ghost-to-normal-event g event-queue level))
                 the-ghosts)))
      ;;
      ;; Scare all Ghosts
      ;;
      (for-each
       (lambda (g)
         (let ((state (g 'get-state)))
           (if (eq? state 'strong)
               (g 'set-state! 'weak))))
       the-ghosts)
      ;;
      ;; Schedule the Ghosts' recovery
      ;;
      (for-each
       (lambda (e)
         (e 'stop)
         (e 'start))
       *ghost-to-normal-events*)))
  
  ;;; ghost-ghost-home : Ghost Ghost-Home Event-Queue number -> void
  ;;;______________________________________________________________________
  (define (ghost-ghost-home ghost ghost-home event-queue level)
    (if (eq? (ghost 'get-state) 'dead)
        (ghost 'set-state! 'strong)))
  
  ;;; A helper
  ;;;______________________________________________________________________
  
  ;;; make-ghost-to-normal-event : Ghost Event-Queue number -> void
  ;;;______________________________________________________________________
  (define (make-ghost-to-normal-event ghost event-queue level)
    (make-event
     (lambda (g)
       (if (eq? (g 'get-state) 'weak)
           (g 'set-state! 'strong)))
     ghost
     GHOST-WEAK-TICKS
     event-queue
     #t))
  
  )
  
  
       

           