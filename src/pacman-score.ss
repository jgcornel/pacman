;;;
;;; pacman-score.ss
;;;
;;; Jan Cornelis
;;; 2007-2008
;;;
;;; This module provides an ADT for a high-scores scoreboard
;;;
;;; A scoreboard is created given a file-name to keep the high scores
;;; The messages that can be sent to a score-board are the expected ones:
;;;
;;;  'display: displays the score-board on the screen
;;;  'add-score: adds a score to the high-scores list
;;;
;;; Note that when a score is added to the high scores list the new list
;;; is written to the file with the given file-name
;;;
;;; The high scores file keeps the high scores in a simple text format.
;;; It is thus very easy to tamper with the high scores list. It is however
;;; important to realize that the scoreboard module was the last module 
;;; created for the Pacman software project and that it only serves as
;;; proof of concept.
;;;________________________________________________________________________

(module pacman-score mzscheme
  
  (require "pacman-util.ss"
           "pacman-glob.ss"
           "pacman-gui.ss")
  
  (provide make-scoreboard) ; string -> ScoreBoard
  
  ;;; Passive Score ADT
  ;;;______________________________________________________________________
  
  (define (make-score name points)
    (cons name points))
  
  (define (score-name-ref score)
    (car score))
  
  (define (score-points-ref score)
    (cdr score))
  
  (define (score<? score1 score2)
    (< (score-points-ref score1) (score-points-ref score2)))
  
  ;;; ScoreBoard ADT
  ;;;
  ;;; Messages:
  ;;;
  ;;;  'display   :               -> void
  ;;;  'add-score : string number -> void
  ;;;______________________________________________________________________
  (define (make-scoreboard file-name)
    (let ((score-list #f)
          (score-matrix #f))
      ;;;
      ;;; helper
      ;;;
      (define (initialize)
        (set! score-list (file->scores file-name))
        (set! score-matrix (scores->matrix score-list)))
      ;;;
      ;;; interface functions
      ;;;
      (define (_display)
        (matrix-for-each
         score-matrix
         (lambda (mat row col)
           (let* ((pos (make-position col row))
                  (char (matrix-ref mat row col))
                  (image-file 
                   (character->image-file char IMAGE-DIRECTORY IMAGE-TYPE)))
             (if image-file
                 (draw-image pos image-file)
                 (draw-nothing pos))))))
      (define (add-score name points)
        (let* ((short-name 
                (if (> (string-length name) MAX-NAME-SIZE)
                    (substring name 0 MAX-NAME-SIZE)
                    name))
               (score (make-score short-name points)))
          (set! score-list (_add-score score-list score))
          (set! score-matrix (scores->matrix score-list))
          (save file-name score-list)))
      ;;;
      ;;; dispatch
      ;;;
      (define (dispatch m . args)
        (cond
          ((eq? m 'display)   (apply _display args))
          ((eq? m 'add-score) (apply add-score args))
          (else (error "PACMAN-SCOREBOARD -- unknown message:" m))))
      ;;;
      ;;; initialize and return dispatch
      ;;;
      (initialize)
      dispatch))
  
  ;;; Helper functions
  ;;;______________________________________________________________________

  ;;; file->scores : string -> list
  ;;; Returns a list of scores based on a filename
  ;;;______________________________________________________________________
  (define (file->scores file-name)
    (define (collect scores port)
      (let ((next-name (read port))
            (next-points (read port)))
        (if (eof-object? next-points)
            (reverse scores)
            (collect (cons (make-score next-name next-points) scores) port))))
    (if (file-exists? file-name)
        (let* ((port (open-input-file file-name))
               (scores (collect '() port)))
          (close-input-port port)
          scores)
        '()))
  
  ;;; scores->matrix : list -> Matrix
  ;;; Returns a Matrix of scores based on a list of Scores
  ;;;______________________________________________________________________
  (define (scores->matrix scores)
    (let ((matrix (make-matrix SCORE-BOARD-SIZE SCORE-BOARD-SIZE #\space))
          (title-string 
           (string-pad-right SCORE-BOARD-TITLE-STRING SCORE-BOARD-SIZE)))
      ;;
      ;; a helper's helpers...
      ;;
      (define (string-in-row string the-row)
        (matrix-row-list
         matrix
         the-row
         (lambda (mat row col val) (matrix-set! mat row col val))
         (string->list string)))
      (define (handle-scores todo-scores rank row)
        (if (not (null? todo-scores))
            (let* ((score (car todo-scores))
                   (name  (score-name-ref score))
                   (points (score-points-ref score))
                   (a-string 
                    (make-score-string rank name points SCORE-BOARD-SIZE)))
              (string-in-row a-string row)
              (handle-scores (cdr todo-scores) (+ rank 1) (+ row 1)))))
      ;;
      ;; populate and return matrix
      ;;
      (string-in-row title-string 0)
      (handle-scores (reverse scores) 1 3)
      matrix))
  
  ;;; _add-score : list Score -> list
  ;;; Returns a list of Scores that results from adding a given Score to a
  ;;; given list of Scores
  ;;;______________________________________________________________________
  (define (_add-score scores score)
    (define (insert-into curr-scores a-score)
      (cond 
        ((null? curr-scores) (list a-score))
        ((score<? a-score (car curr-scores)) (cons a-score curr-scores))
        (else (cons (car curr-scores) (insert-into (cdr curr-scores) a-score)))))
    (let* ((new-scores (insert-into scores score))
           (new-length (length new-scores)))
      (if (> new-length SCORE-BOARD-LENGTH)
          (cdr new-scores)
          new-scores)))
  
  ;;; save : string list -> void
  ;;; Saves a given list of Scores in a file with a given filename
  ;;;______________________________________________________________________
  (define (save file-name scores)
    (define (traverse todo-scores port)
      (if (not (null? todo-scores))
          (let* ((score (car todo-scores))
                 (name  (score-name-ref score))
                 (points (score-points-ref score)))
            (write name port)
            (write points port)
            (traverse (cdr todo-scores) port))))
    (let ((port (open-output-file file-name 'truncate)))
      (traverse scores port)
      (close-output-port port)))
  
  ;;; make-score-string : number string number number -> string
  ;;; Returns a string to be displayed on the screen based on the given
  ;;; parameters, for example :
  ;;;    (make-score 1 "Jan Cornelis" 329050 25)
  ;;; will return
  ;;;    " 1 Jan Cornelis    329050"
  ;;;______________________________________________________________________
  (define (make-score-string rank name points len)
    (let* ((rank-string (string-pad (number->string rank) 2))
           (left-string (string-append rank-string " " name))
           (right-string (number->string points)))
      (split-strings left-string right-string len)))
  
  )