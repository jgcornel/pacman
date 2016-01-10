;;;
;;; pacman-file.ss
;;; 
;;; Jan Cornelis
;;; 2007-2008
;;;
;;; This module provides the function file->maze
;;; This is a helper function that will be used by the main Pacman program
;;; to create the Pacman Maze based upon a simple text file. This text file
;;; should contain the same number of "characters" on every line. If this
;;; is the case, the characters are interpreted as follows:
;;;
;;;  * #\space : Ignored
;;;  * #\D     : Display
;;;  * #\P     : Path
;;;  * #\W     : Wall
;;;  * #\c     : Cookie
;;;  * #\e     : Energizer
;;;  * #\F     : Path where fruit will (dis)appear
;;;  * #\p     : Pacman
;;;  * #\g     : Ghost
;;;  * default : Path
;;;
;;; To determine the color and the AI function that should be assigned to
;;; a Ghost, the functions GHOST-COLOR-FUNCTION and GHOST-AI-FUNCTION are
;;; used. These functions will assign a color and a function from the 
;;; parameters used to create these functions in the pacman-glob.ss module 
;;; in a round robin manner. For example if 5 ghosts are created their 
;;; colours might be:
;;;
;;;   "Red", "Pink", "Orange", "Cyan" and "Red"
;;;
;;;________________________________________________________________________

(module pacman-file mzscheme
  
  (require (lib "list.ss")
           "pacman-util.ss"
           "pacman-glob.ss"
           "pacman-maze.ss")
  
  (provide file->maze)      ; string -> Maze / #f
  
  ;;; file->maze : string -> Maze / #f
  ;;;______________________________________________________________________
  (define (file->maze file-name)
    (let ((dimensions (file->dimensions file-name)))
      (define (filler port maze row)
        (let ((line (read-line port)))
          (if (not (eof-object? line))
              (begin
                (populate-maze maze row line)
                (filler port maze (+ row 1))))))
      (if dimensions          
          (let* ((rows (car dimensions))
                 (cols (cdr dimensions))
                 (port (open-input-file file-name))
                 (maze (make-maze rows cols)))
            (filler port maze 0)
            (close-input-port port)
            (maze 'for-each 
                  (lambda (cell)
                    (if (eq? (cell 'get-type) 'wall)
                        (shape-wall cell maze))))
            maze)          
          #f)))
  
  ;;; Helpers
  ;;;______________________________________________________________________
  
  ;;; file->dimensions : string -> pair / #f
  ;;; Calculate and return the dimensions of the Maze
  ;;; (car pair) corresponds to the number of rows
  ;;; (cdr pair) corresponds to the number of columns
  ;;; #f is returned if not all lines contain the same number of alphabetic
  ;;; characters
  ;;;______________________________________________________________________
  (define (file->dimensions file-name)
    (define (iter port row number-of-cols)
      (let ((line (read-line port)))
        (if (not (eof-object? line))
            (let ((new-number-of-cols 
                   (length (filter char-alphabetic? (string->list line)))))
              (if (not number-of-cols)
                  (set! number-of-cols new-number-of-cols))
              (if (not (= new-number-of-cols number-of-cols))
                  #f
                  (iter port (+ row 1) new-number-of-cols)))
            (cons row number-of-cols))))
    (let* ((port (open-input-file file-name))
           (dimensions (iter port 0 #f)))
      (close-input-port port)
      dimensions))
  
  ;;; shape-wall : Wall Maze -> void
  ;;; Determines the shape of a Wall by inspecting the surrounding Cells in
  ;;; Maze. Remember that a Direction will be added to the Wall if the Cell
  ;;; in this Direction is also a Wall.
  ;;;______________________________________________________________________
  (define (shape-wall wall maze)
    (for-each
     (lambda (dir)
       (let* ((pos (wall 'get-position))
              (neighbour-pos (position-in-direction pos dir)))
         (if (maze 'contains-position? neighbour-pos)
             (let* ((cell (maze 'get neighbour-pos))
                    (type (cell 'get-type)))
               (if (eq? type 'wall)
                   (wall 'add-direction dir))))))
     (list north south east west)))
  
  ;;; populate-maze : Maze number string -> void
  ;;; 
  ;;;______________________________________________________________________
  (define (populate-maze maze row line)
    (define (character-apply character index)
      (let ((position (make-position index row)))
        (cond
          ;; WALL
          ((equal? character #\W)
           (maze 'set! position (make-wall position '())))
          ;; DISPLAY
          ((equal? character #\D)
           (maze 'set! position (make-display position #\space)))
          ;; PATH
          ((equal? character #\P)
           (maze 'set! position (make-path position #f #f '())))
          ;; FRUIT-POSITION
          ((equal? character #\F)
           (maze 'set! position (make-path position #f #f '()))
           (maze 'set-fruit-position! position))
          ;; COOKIE
          ((equal? character #\c)
           (let ((cookie 
                  (make-reward 
                   position 'cookie COOKIE-FILE COOKIE-SCORE)))
             (maze 'set! position (make-path position cookie #f '()))))
          ;; ENERGIZER
          ((equal? character #\e)
           (let ((energizer 
                  (make-reward 
                   position 'energizer ENERGIZER-FILE ENERGIZER-SCORE)))
             (maze 'set! position (make-path position energizer #f '()))))
          ;; PACMAN
          ((equal? character #\p)
           (let ((pacman 
                  (make-pacman position PACMAN-DIRECTION maze PACMAN-LIVES)))
             (maze 'set! position (make-path position #f pacman '()))
             (maze 'set-the-pacman! pacman)))
          ;; GHOST
          ((equal? character #\g)
           (let ((ghost (make-ghost position 
                                    GHOST-DIRECTION 
                                    maze 
                                    (GHOST-COLOR-FUNCTION) 
                                    (GHOST-AI-FUNCTION)))
                 (ghost-home 
                  (make-reward position 'ghost-home GHOST-HOME-FILE 0)))
             (maze 'set! 
                   position 
                   (make-path position ghost-home #f (list ghost)))
             (maze 'add-ghost ghost)))
          ;; DEFAULT: PATH
          (else (maze 'set! position (make-path position #f #f '()))))))
    (define (iter lst index)
      (if (not (null? lst))
          (begin
            (character-apply (car lst) index)
            (iter (cdr lst) (+ index 1)))))
    (iter (filter char-alphabetic? (string->list line)) 0))  
  
  )