;;;
;;; pacman.scm
;;; 
;;; Jan Cornelis
;;; 2007-2008
;;;
;;; This is the main Pacman program
;;; It uses the pacman modules and brings them together to make a working
;;; program. It carries out the following functions:
;;;
;;;  * Manage the windows and menus
;;;  * Handle the user input
;;;
;;; Part of handling the user input is starting a game. In order to do
;;; this a thread is started that runs a game-controller. This thread
;;; will start another thread that runs until either Pacman got killed,
;;; the level was finished or the game is over. The game-controller thread
;;; will then take whatever action is appropriate to continue the game.
;;;
;;;________________________________________________________________________

(require "pacman-util.ss"
         "pacman-glob.ss"
         "pacman-gui.ss"
         "pacman-file.ss"
         "pacman-score.ss"
         "pacman-maze.ss"
         "pacman-game-loop.ss")

;;;
;;; my-canvas%
;;;________________________________________________________________________
(define my-canvas%
  (class canvas%
    (define/override (on-char evt)
      (handle-key (send evt get-key-code)))
    (super-new)))

;;;
;;; Frame, Panel, Canvas, Menu and Dialog windows
;;;________________________________________________________________________
(define pacman-frame 
  (instantiate frame% ("Pacman") 
    (width WINDOW-WIDTH) (height WINDOW-HEIGHT)))

(define pacman-panel (instantiate vertical-panel% (pacman-frame)))

(define pacman-canvas
  (instantiate my-canvas% (pacman-panel)
    (style '(border no-autoclear))
    (paint-callback (lambda (c d) (refresh-gui)))))

(define menu-bar (instantiate menu-bar% (pacman-frame)))

(define file-menu (new menu% (label FILE-STRING) (parent menu-bar)))

(define open-file-menu-item
  (make-object menu-item% OPEN-STRING file-menu
    (lambda (_1 _2) (load-maze))))

(define exit-menu-item
  (make-object menu-item% EXIT-STRING file-menu
    (lambda (_1 _2) 
      (stop-game)
      (send pacman-frame show #f))))

(define game-menu (new menu% (label GAME-STRING) (parent menu-bar)))

(define start-game-menu-item
  (make-object menu-item% START-STRING game-menu
    (lambda (_1 _2) (start-game))))

(define stop-game-menu-item
  (make-object menu-item% STOP-STRING game-menu
    (lambda (_1 _2) (stop-game))))

(define high-scores-menu-item
  (make-object menu-item% HIGH-SCORES-STRING game-menu
    (lambda (_1 _2) (show-high-scores))))

(define help-menu (new menu% (label HELP-STRING) (parent menu-bar)))

(define help-menu-item
  (make-object menu-item% HOWTO-STRING help-menu
    (lambda (_1 _2) (show-how-to))))

(define about-menu-item
  (make-object menu-item% ABOUT-STRING help-menu
    (lambda (_1 _2) (show-about-pacman))))

(define game-over-dialog (instantiate dialog% (GAME-OVER-STRING)))
(instantiate text-field% 
  (YOUR-NAME-STRING game-over-dialog (lambda (tf e) (set-player-name! tf e))))
(define game-over-panel
  (instantiate horizontal-panel% (game-over-dialog)
    (alignment '(center center))))
(instantiate button% (OK-STRING game-over-panel (lambda (b e) (store-high-score b e))))

(define invalid-file-dialog (instantiate dialog% (INVALID-FILE-STRING)))
(define invalid-file-panel
  (instantiate horizontal-panel% (invalid-file-dialog)
    (alignment '(center center))))
(instantiate button% 
  (OK-STRING invalid-file-panel (lambda (b e) (send invalid-file-dialog show #f))))

;;;
;;; MAIN GAME
;;; THE GLOBAL VARIABLES WILL BE MANIPULATED BY THE CALLBACK FUNCTIONS
;;;
;;;________________________________________________________________________

(define *game-maze* #f)
(define *orig-maze* #f)
(define *player-name* #f)
(define *score-board* (make-scoreboard SCORE-BOARD-FILE))
(define *game-loop-thread* #f)
(define *game-controller-thread* #f)
(define *game-flag* (make-flag 'continue))
(define *drawing-context* (send pacman-canvas get-dc))

(initialize-gui *drawing-context* 1 1)
(send pacman-frame show #t)
(sleep/yield 1)
(draw-image 
 (make-position 0 0)
 (string-append IMAGE-DIRECTORY TITLE-FILE))
(refresh-gui)

;;; Call-back functions
;;;________________________________________________________________________

;;; handle-key : char -> void
;;;________________________________________________________________________
(define (handle-key key)
  (if *game-controller-thread*
      (let ((pacman (*game-maze* 'get-the-pacman)))
        (case key
          ((up)    (pacman 'set-direction! north))
          ((down)  (pacman 'set-direction! south))
          ((left)  (pacman 'set-direction! west))
          ((right) (pacman 'set-direction! east))
          ((#\space)
           (if *game-loop-thread*
               (if (thread-running? *game-loop-thread*)
                   (begin
                     (thread-suspend *game-loop-thread*)
                     (*game-maze*
                      'set-string!
                      (string-pad-right 
                       CONTINUE-STRING
                       (*game-maze* 'number-of-cols)) 0))
                   (thread-resume *game-loop-thread*)))))
        (*game-maze* 'display))))
        ;(refresh-gui))))
  
;;; load-maze : -> void
;;;________________________________________________________________________
(define (load-maze)
  (let ((fn (get-file)))
    (when fn
      (set! *game-maze* (file->maze fn))
      (set! *orig-maze* (file->maze fn))
      (if (not *game-maze*)
          (send invalid-file-dialog show #t)
          (let ((rows (*game-maze* 'number-of-rows))
                (cols (*game-maze* 'number-of-cols)))
            (initialize-gui *drawing-context* rows cols)
            (*game-maze*
             'set-string!
             (string-pad-right
              READY-STRING
              cols) 0)
            (*game-maze* 'display)
            (refresh-gui))))))

;;; start-game : -> void
;;;________________________________________________________________________
(define (start-game)
  (if *game-maze*
      (let ((game-controller 
             (make-game-controller *game-maze* *orig-maze* 1 *game-flag*)))
        (set! *game-controller-thread* (thread game-controller)))))

;;; stop-game : -> void
;;;________________________________________________________________________
(define (stop-game)
  (if *game-controller-thread*
      (kill-thread *game-controller-thread*))
  (if *game-loop-thread*
      (kill-thread *game-loop-thread*))
  (set! *game-controller-thread* #f)
  (set! *game-loop-thread* #f))

;;; show-high-scores : -> void
;;;________________________________________________________________________
(define (show-high-scores)
  (initialize-gui *drawing-context* SCORE-BOARD-SIZE SCORE-BOARD-SIZE)
  (*score-board* 'display)
  (refresh-gui))

(define (show-how-to)
  void)

(define (show-about-pacman)
  void)

(define (set-player-name! tf e)
  (set! *player-name* (send tf get-value)))

(define (store-high-score b e)
  (let* ((pacman (*game-maze* 'get-the-pacman))
         (points (pacman 'get-score)))
    (*score-board* 'add-score *player-name* points)
    (send game-over-dialog show #f)
    (sleep GRACE-TIME)
    (show-high-scores)))

;;; make-game-controller : Maze Maze number dialog% -> procedure
;;;________________________________________________________________________
(define (make-game-controller maze1 maze2 level dialog)
  (letrec
     ((game-controller
       (lambda ()
         ;;
         ;; Set up the game elements and start
         ;;
         (define pacman (maze1 'get-the-pacman))
         (define ghosts (maze1 'get-the-ghosts))
         (define cols   (maze1 'number-of-cols))
         (define game-loop (make-game-thread maze1 level *game-flag*))
         (set! *game-loop-thread* (thread game-loop))
         (thread-wait *game-loop-thread*)
         (sleep/yield GRACE-TIME)
         ;;
         ;; The game's continuation depends on *game-flag*
         ;;
         (case (flag-ref *game-flag*)
           
           ('in-level
             (for-each
              (lambda (a)
                (a 'go-home)
                (a 'set-state! 'strong))
              (cons pacman ghosts))
             (maze1 'set-string! (string-pad-right READY-STRING cols) 0)
             (maze1 'display)
             (refresh-gui)
             (sleep/yield GRACE-TIME)
             (game-controller))
           
           ('between-levels
             (for-each
              (lambda (a)
                (a 'go-home)
                (a 'set-state! 'strong))
              (cons pacman ghosts))
             (fill-maze maze1 maze2)
             (set! level (+ level 1))
             (maze1 'set-string! 
                    (string-pad-right 
                     (string-append 
                      NEXT-LEVEL-STRING 
                      (number->string level))
                     cols) 0)
             (maze1 'display)
             (refresh-gui)
             (sleep/yield GRACE-TIME)
             (game-controller))
           
           ('game-over
             (send game-over-dialog show #t))))))
    
    game-controller))

;;; fill-maze : Maze Maze -> void
;;;________________________________________________________________________
(define (fill-maze maze1 maze2)
  (maze2 
   'for-each
   (lambda (cell)
     (if (eq? (cell 'get-type) 'path)
         (let ((reward (cell 'get-reward))
               (position (cell 'get-position)))
           (if reward
               (let ((orig-cell (maze1 'get position)))
                 (orig-cell 'set-reward! reward))))))))