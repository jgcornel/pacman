;;;
;;; pacman-game-loop.ss
;;; 
;;; Jan Cornelis
;;; 2007-2008
;;;
;;; This module provides only one procedure: make-game-thread
;;;
;;; make-game-thread
;;; -----------------
;;; Returns a procedure of no arguments. The returned procedure will be
;;; run as a thread. If the thread exits, it will update a Flag that is 
;;; given as a procedure to signal the reason why it has exited. Possible
;;; reasons are:
;;;
;;;  * 'between-levels: all rewards are eaten, the game should move to
;;;                     the next level.
;;;  * 'in-level: Pacman was killed, but still has some lives.
;;;               The game should simply restart within the current level.
;;;  * 'game-over: Pacman was killed and has no more lives left
;;;                The game is over.
;;;
;;; Scheduling of events is done using an Event-Queue. The following events
;;; will be created and scheduled:
;;;
;;;  * pacman-move-event: Pacman moving.
;;;  * ghost-move-events: the Ghosts moving.
;;;  * fruit-appear-event: a Fruit appearing in the Maze.
;;;  * fruit-disappear-event: a Fruit disappearing in the Maze.
;;;
;;; Determining and applying the impact of the events that took place, is
;;; done using the functions that were looked up using game-logic-lookup.
;;;
;;; make-game-thread is one huge function with a lot of helper functions
;;;________________________________________________________________________

(module pacman-game-loop mzscheme
  
  (require (lib "list.ss")
           "pacman-util.ss"
           "pacman-event.ss"
           "pacman-glob.ss"
           "pacman-gui.ss"
           "pacman-maze.ss"
           "pacman-game-logic.ss")
  
  (provide make-game-thread) ; Maze number Flag -> procedure
  
  ;;; make-game-thread : Maze number Flag -> procedure
  ;;;______________________________________________________________________
  (define (make-game-thread maze level flag)
    ;;
    ;; All of this is needed for our game-loop
    ;;
    (let* ((game-event-queue (make-event-queue EVENT-QUEUE-SIZE))
           (fruit-position (maze 'get-fruit-position))
           (pacman (maze 'get-the-pacman))
           (ghosts (maze 'get-the-ghosts))
           (ghost-move-events '())
           (pacman-move-event #f)           
           (impacted-ghosts '())
           (fruit #f))
      ;;
      ;; make-pacman-move-event : -> Event
      ;;___________________________________________________________________
      (define (make-pacman-move-event)
        (make-event
         (lambda (pac)
           (pac 'move)
           ;;
           ;; Add every Ghost that Pacman met to the list of impacted-ghosts.
           ;; This list was added to fix the crossing agents problem.
           ;;
           (for-each
            (lambda (g)
              (if (position=? (pac 'get-position) (g 'get-position))
                  (set! impacted-ghosts (add-to g impacted-ghosts))))
            ghosts))
         pacman
         PACMAN-MOVE-TICKS
         game-event-queue
         #f)) ; recurring
      ;;
      ;; make-ghost-move-events : -> list
      ;;___________________________________________________________________
      (define (make-ghost-move-events)
        (map
         (lambda (ghost)
           (make-event
            (lambda (g)
              (g 'move)
              ;;
              ;; Add every Ghost that Pacman met to the list of impacted-ghosts.
              ;; This list was added to fix the crossing agents problem.
              ;;
              (if (and (position=? (pacman 'get-position) (g 'get-position))
                       (direction=? (pacman 'get-direction)
                                    (direction-opposite (ghost 'get-direction))))
                  (set! impacted-ghosts (add-to g impacted-ghosts))))
            ghost
            GHOST-MOVE-TICKS
            game-event-queue
            #f)) ; recurring         
         ghosts))
      ;;
      ;; make-fruit
      ;;
      (define (make-fruit)
        (if fruit-position
            (make-reward fruit-position 
                         'fruit 
                         (FRUIT-FILE-FUNCTION)
                         (* FRUIT-SCORE level))
            #f))
      ;;
      ;; make-fruit-appear-event
      ;;
      (define (make-fruit-appear-event)
        (make-event
         (lambda (f)
           (let ((cell (maze 'get fruit-position))
                 (fruit-disappear-event (make-fruit-disappear-event)))
             (cell 'set-reward! fruit)
             (fruit-disappear-event 'start)))
         fruit
         FRUIT-PRESENT-TICKS
         game-event-queue
         #t)) ; only-once
      ;;
      ;; make-fruit-disappear-event
      ;;
      (define (make-fruit-disappear-event)
        (make-event
         (lambda (f)
           (let ((cell (maze 'get fruit-position))
                 (fruit-appear-event (make-fruit-appear-event)))
             (cell 'set-reward! #f)
             (fruit-appear-event 'start)))
         fruit
         FRUIT-HIDDEN-TICKS
         game-event-queue
         #t)) ; only once
      ;;
      ;; apply-impacts
      ;;
      (define (apply-impacts)
        (define (find-and-apply obj1 obj2)
          (let ((proc (game-logic-lookup (obj1 'get-type) (obj2 'get-type))))
            (if proc
                (proc obj1 obj2 game-event-queue level))))
        ;
        ; Agents might have met Rewards
        ;
        (for-each
         (lambda (a)
           (let* ((pos (a 'get-position))
                  (cell (maze 'get pos))
                  (reward (cell 'get-reward)))
             (if reward
                 (find-and-apply a reward))))
         (cons pacman ghosts))

        ;
        ; add the ghosts Pacman met to impacted-ghosts
        ;        
        (set! 
         impacted-ghosts 
         (objects-union 
          impacted-ghosts
          (filter
           (lambda (g)
             (position=? (g 'get-position) (pacman 'get-position)))
           ghosts)))                        
        
        ;
        ; Each Ghost in impacted-ghosts has met Pacman
        ;
        (for-each
         (lambda (g)
           (find-and-apply pacman g))
         impacted-ghosts)
        ;
        ; Find out how the game should go on
        ;
        (let
            ((number-of-rewards
              (apply 
               +
               (map
                (lambda (c)
                  (if (eq? (c 'get-type) 'path)
                      (let ((reward (c 'get-reward)))
                        (if reward
                            (if (not (eq? (reward 'get-type) 'ghost-home))
                                1
                                0)
                            0))
                      0))
                (maze 'to-list))))
             (pacman-state (pacman 'get-state)))
          (cond
            ; No rewards are left => go to the next level
            ((= number-of-rewards 0)  'between-levels)
            ; Pacman was killed but still has some lives => resume the game
            ((eq? pacman-state 'weak) 'in-level) 
            ; Pacman was killed and has no more lives => game over
            ((eq? pacman-state 'dead) 'game-over)
            ; Nothing special happened => just continue
            (else 'continue))))
      ;;
      ;; make-score-string
      ;; Return a string to display the status of the game in the Display row
      ;;
      (define (make-score-string)
        (let* ((cols (maze 'number-of-cols))
               (score (pacman 'get-score))
               (lives (pacman 'get-lives))
               (score-string (string-append SCORE-STRING (number->string score)))
               (lives-string (string-append LIVES-STRING (number->string lives))))
          (split-strings score-string lives-string cols)))
      ;;
      ;; game-loop
      ;;
      (define (game-loop)
        (set! impacted-ghosts '())
        (game-event-queue 'handle-due-events)
        (game-event-queue 'countdown)
        (let ((continue (apply-impacts)))
          (maze 'set-string! (make-score-string) 0) ;++ YUCKS
          (maze 'display)
          (if (eq? continue 'continue)
              (begin
                (sleep GAME-LOOP-SLEEP-TIME)
                (game-loop))
              (flag-set! flag continue))))
      ;;
      ;; Initialize some stuff and return the game-loop procedure
      ;;
      (game-logic-install)
      (set! fruit (make-fruit))
      (if fruit
          ((make-fruit-appear-event) 'start))
      (for-each
       (lambda (e)
         (e 'start))
       (cons (make-pacman-move-event)
             (make-ghost-move-events)))
      game-loop))
  
  )