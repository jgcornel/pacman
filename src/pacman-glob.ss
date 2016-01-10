;;;
;;; pacman-glob.ss
;;;
;;; Jan Cornelis
;;; 2007-2008
;;;
;;; This module centralizes all constants that are used throughout the
;;; Pacman game. Some constants will determine the game's look and feel,
;;; while others will steer it's behaviour. All strings that are used by
;;; the program are also defined as constants. This allows for an easy
;;; translation of the program.
;;;
;;; Ideally some of the "constants" defined in this place should be 
;;; configurable via some initialization file or via a menu. Like that
;;; the users could change the colors of the game or switch to another
;;; language. Nevertheless this possibility has not been explored in
;;; practice.
;;;
;;;________________________________________________________________________

(module pacman-glob mzscheme
  
  (require "pacman-util.ss"
           "pacman-ai.ss")
  
  (provide WINDOW-WIDTH             ; original width of the program window
           WINDOW-HEIGHT            ; original height of the program window
           GRACE-TIME               ; small time between screens
           BACKGROUND-COLOR         ; color of the maze's background
           WALL-COLOR               ; color of a wall
           PACMAN-COLOR             ; color of pacman
           WEAK-GHOST-COLOR         ; color of a weak ghost
           GHOST-MOUTH-COLOR        ; color of a ghost's mouth
           GHOST-EYEWHITE-COLOR     ; color of a ghost's eyewhite
           GHOST-EYEPUPIL-COLOR     ; color of a ghost's pupil
           GHOST-COLOR-FUNCTION     ; function that gives back a color
           IMAGE-DIRECTORY          ; directory in which images are kept
           IMAGE-TYPE               ; type of images kept
           COOKIE-FILE              ; file containing a cookie image
           ENERGIZER-FILE           ; file containing an energizer image
           GHOST-HOME-FILE          ; file containing a ghost home image
           TITLE-FILE               ; file containing a welcome / title image
           FRUIT-FILE-FUNCTION      ; function that returns a file containing a fruit image
           PACMAN-DIRECTION         ; initial direction of pacman
           GHOST-DIRECTION          ; initial direction of a ghost
           PACMAN-LIVES             ; initial number of lives of pacman
           COOKIE-SCORE             ; score for eating a cookie
           ENERGIZER-SCORE          ; score for eating an energizer
           FRUIT-SCORE              ; base-score for eating a fruit
           DEAD-GHOST-SCORE         ; base-score for killing a ghost
           GAME-LOOP-SLEEP-TIME     ; time between two iterations of the game
           GHOST-WEAK-TICKS         ; number of iterations a ghost remains weak
           PACMAN-MOVE-TICKS        ; number of iterations between two pacman moves
           GHOST-MOVE-TICKS         ; number of iterations between two ghost moves
           FRUIT-PRESENT-TICKS      ; number of iterations a fruit will be present in the maze
           FRUIT-HIDDEN-TICKS       ; number of iterations a fruit will be hidden in the maze
           GHOST-AI-FUNCTION        ; function that returns an AI function for the ghost to move
           EVENT-QUEUE-SIZE         ; size of the event-queue used by the game-loop
           MAX-NAME-SIZE            ; maximum size of a name in the high-scores list
           SCORE-BOARD-SIZE         ; size (height = width) of the score-board size
           SCORE-BOARD-LENGTH       ; maximum number of entries in the high-scores list
           SCORE-BOARD-FILE         ; file to keep the high-scores list
           FILE-STRING              ; string for the File menu
           OPEN-STRING              ; string for the File - Open menu item
           EXIT-STRING              ; string for the File - Exit menu item
           GAME-STRING              ; String for the Game menu
           START-STRING             ; String for the Game - Start menu item
           STOP-STRING              ; String for the Game - Stop menu item
           HIGH-SCORES-STRING       ; String for the Game - High Scores menu item
           HELP-STRING              ; String for the Help menu
           HOWTO-STRING             ; String for the Help - How to play? menu item
           ABOUT-STRING             ; String for the Help - About Pacman menu item
           GAME-OVER-STRING         ; String for the Game Over dialog box
           YOUR-NAME-STRING         ; String for the Game Over dialog box
           OK-STRING                ; OK string
           INVALID-FILE-STRING      ; String for the Invalid file dialog box
           SCORE-STRING             ; String for the display of the score
           LIVES-STRING             ; String for the display of the number of lives
           SCORE-BOARD-TITLE-STRING ; String for the display of the high scores
           CONTINUE-STRING          ; String to display when the game is paused
           READY-STRING             ; String to display when the game is resumed 
           NEXT-LEVEL-STRING        ; String to display when the next level is started
   )
  
  ;;;
  ;;; Constants that govern the look and feel of the program
  ;;;______________________________________________________________________
  
  (define fruit-file-list 
    (list "strawberry.png" "cherry.png" "banana.png"))
  (define ghost-colors    
    (list "Red" "Pink" "Orange" "Cyan"))
  
  (define         WINDOW-WIDTH 640)
  (define        WINDOW-HEIGHT 420)
  (define           GRACE-TIME 1)
  (define     BACKGROUND-COLOR "White")
  (define           WALL-COLOR "Red")
  (define         PACMAN-COLOR "Yellow")
  (define     WEAK-GHOST-COLOR "Blue")
  (define    GHOST-MOUTH-COLOR "White")
  (define GHOST-EYEWHITE-COLOR "White")
  (define GHOST-EYEPUPIL-COLOR "Black")
  (define GHOST-COLOR-FUNCTION (apply make-round-robin ghost-colors)) 
  
  (define      IMAGE-DIRECTORY "img/")
  (define           IMAGE-TYPE 'png)
  (define          COOKIE-FILE "cookie.png")
  (define       ENERGIZER-FILE "energizer.png")
  (define      GHOST-HOME-FILE "ghost-home.png")
  (define           TITLE-FILE "title.png")
  (define  FRUIT-FILE-FUNCTION (apply make-round-robin fruit-file-list))
  
  ;;;
  ;;; Constants that govern the rules of the game
  ;;;______________________________________________________________________
  
  (define PACMAN-DIRECTION west)
  (define  GHOST-DIRECTION north)
  (define     PACMAN-LIVES 4)
  (define     COOKIE-SCORE 10)
  (define  ENERGIZER-SCORE 100)
  (define      FRUIT-SCORE 100)
  (define DEAD-GHOST-SCORE 200)  

  ;;;
  ;;; Constants that govern the behaviour of the program
  ;;;______________________________________________________________________  

  (define ghost-functions 
    (list (make-random-mover) (make-mts-mover) (make-jc-mover)))
  
  (define GAME-LOOP-SLEEP-TIME 0.05)
  (define     GHOST-WEAK-TICKS 50)
  (define    PACMAN-MOVE-TICKS 2)
  (define     GHOST-MOVE-TICKS 3)
  (define  FRUIT-PRESENT-TICKS 50)
  (define   FRUIT-HIDDEN-TICKS 150)
  (define    GHOST-AI-FUNCTION (apply make-round-robin ghost-functions))
  (define     EVENT-QUEUE-SIZE 100)
  
  ;;;
  ;;; Constants that govern the look and feel of the Scoreboard
  ;;;______________________________________________________________________
  
  (define MAX-NAME-SIZE      15)
  (define SCORE-BOARD-SIZE   25)
  (define SCORE-BOARD-LENGTH 20)
  (define SCORE-BOARD-FILE   "scores.txt")
  
  ;;;
  ;;; Strings that can be changed for internationalization of the program
  ;;;______________________________________________________________________
  
  (define              FILE-STRING "File")
  (define              OPEN-STRING "Open")
  (define              EXIT-STRING "Exit")
  (define              GAME-STRING "Game")
  (define             START-STRING "Start")
  (define              STOP-STRING "Stop")
  (define              HELP-STRING "Help")
  (define             HOWTO-STRING "How to play")
  (define             ABOUT-STRING "About Pacman")
  (define         GAME-OVER-STRING "Game over")
  (define         YOUR-NAME-STRING "Enter your name: ")
  (define                OK-STRING "OK")
  (define      INVALID-FILE-STRING "Invalid file")
  (define       HIGH-SCORES-STRING "High Scores")
  (define             SCORE-STRING "SCORE:")
  (define             LIVES-STRING "LIVES:")
  (define SCORE-BOARD-TITLE-STRING "HIGH SCORES")
  (define          CONTINUE-STRING "TO CONTINUE PRESS SPACE")
  (define             READY-STRING "ARE YOU READY")
  (define        NEXT-LEVEL-STRING "NEXT LEVEL:")
  
)
  
  
  
  