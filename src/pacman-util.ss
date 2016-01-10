;;; 
;;; pacman-util.ss
;;; 
;;; Jan Cornelis
;;; 2007-2008
;;;
;;; This module is used throughout the entire Pacman project.
;;;
;;; *** A number of ADT's are provided: ***
;;;
;;; Position
;;; --------
;;; A Position consists of 2 numbers x and y and can be used to represent
;;; a point in an XY plane or alternatively an element of a Matrix. In that
;;; case the x-value represents the column-number and the y-value the row-
;;; number.
;;;
;;; Direction
;;; ----------
;;; A Direction consists of 2 numbers delta-x and delta-y and represents a
;;; direction in an XY plane. The constant Directions north, south, east
;;; and west are also provided by this module.
;;;
;;; Matrix
;;; -------
;;; A Matrix represents a matrix. Apart from the expected 
;;; matrix-ref and matrix-set! the procedures matrix-position-ref and
;;; matrix-position-set! are also provided. They make it possible to
;;; use a position to index a matrix.
;;;
;;; Graph
;;; -----
;;; A Graph represents a graph. A Graph is a Matrix in which every element
;;; is a list of positions of cells that are reachable from the cell that
;;; is indexed by the position corresponding to the element. 
;;;
;;; Table
;;; -----
;;; A Table represents a two-dimensional table. The table implementation
;;; has been copied from SICP.
;;;
;;; Flag
;;; ----
;;; A Flag represents a status-flag that will be used to flag the reason a
;;; game has (temporarily) stopped.
;;;
;;; *** A number of useful functions are also provided: ***
;;;
;;; position-in-direction
;;; ---------------------
;;; Returns the Position in an XY plane that will be reached by traveling
;;; one unit from the given Position in the given Direction
;;;
;;; direction-from-to
;;; -----------------
;;; Returns the Direction in an XY plane that will be followed when
;;; traveling from one Position to another
;;;  
;;; matrix-contains-position?
;;; -------------------------
;;; Returns true if a given Matrix can be indexed by a given Position
;;;
;;; remove-from
;;; -----------
;;; Returns the list that would result by removing a given object from a
;;; given list. It uses equal? to determine equality. I (c)(sh)ould have
;;; used the similar function from the "list.ss" library.
;;;
;;; add-to
;;; ------
;;; Returns the list that would result by adding a given object to a given
;;; list. It uses member to determine wether or not the object is already
;;; present. I (c)(sh)ould have used the similar function from the 
;;; "list.ss" library.
;;;
;;; enumerate-interval
;;; ------------------
;;; Returns a list of numbers lying (inclusively) between it's 2 arguments.
;;; This function was copied from SICP.
;;; 
;;; make-round-robin
;;; -----------------
;;; Returns a function that will return one of the arguments given to
;;; make-round-robin in a round-robin fashion.
;;;
;;; make-image-file
;;; ---------------
;;; Takes a directory name and a file name and returns the string 
;;; "directory-name/file-name" if it exists, and #f otherwise
;;;
;;; character->image-file
;;; ----------------------
;;; Takes a character, a directory-name and a file-type and returns the
;;; string "directory-name/file-name" if this file exists and #f otherwise.
;;; The file-name is derived from the character and the file-type. This 
;;; mechanism is crude and might be improved in a future iteration.
;;;
;;; string-pad
;;; ----------
;;; This function is taken from SRFI 13. For more information consult:
;;;   http://srfi.schemers.org/srfi-13/
;;;
;;; string-pad-right
;;; ----------------
;;; This function is taken from SRFI 13. For more information consult:
;;;   http://srfi.schemers.org/srfi-13/
;;;
;;; split-strings
;;; --------------
;;; This function takes two strings and a number and returns a string of
;;; the given length where the two strings occupy opposite ends as in:
;;;
;;; (split strings "een" "twee" 20)
;;; > "een            twee"
;;;
;;; When there is not enough room to fit both strings the second string
;;; takes precedence.
;;;
;;; objects-union
;;; -------------
;;; This function takes two lists of objects and returns a list that is the
;;; union of both lists.
;;;
;;; It should be noted that the error-checking is not very extensive.
;;; The author decided to leave out checking the types of the 
;;; function's arguments to enhance their readibility.
;;;
;;; Comments are minimal as the code is straightforward
;;;________________________________________________________________________

(module pacman-util mzscheme
  
  (require (only (lib "13.ss" "srfi") string-pad string-pad-right))
  
  (provide make-position               ; number number     -> Position
           position?                   ; object            -> boolean
           position-x                  ; Position          -> number
           position-y                  ; Position          -> number
           position=?                  ; Position Position -> boolean
           position-manhattan-distance ; Position Position -> number
           
           make-direction              ; number number       -> Direction
           direction?                  ; object              -> boolean
           direction-delta-x           ; Direction           -> number
           direction-delta-y           ; Direction           -> number
           direction=?                 ; Direction Direction -> boolean
           direction-opposite          ; Direction           -> Direction
           north                       ; Direction constant
           south                       ; Direction constant
           east                        ; Direction constant
           west                        ; Direction constant
           
           make-matrix                 ; number number [object]       -> Matrix
           matrix?                     ; object                       -> boolean
           matrix-number-of-rows       ; Matrix                       -> number
           matrix-number-of-cols       ; Matrix                       -> number
           matrix-ref                  ; Matrix number number         -> object
           matrix-set!                 ; Matrix number number object  -> void
           matrix-position-ref         ; Matrix Position              -> object
           matrix-position-set!        ; Matrix Position object       -> void
           matrix-for-each             ; Matrix procedure             -> void
           matrix-row-list             ; Matrix number procedure list -> void
           matrix->list                ; Matrix                       -> list
           
           make-graph                  ; number number           -> Graph
           graph?                      ; object                  -> boolean
           graph-number-of-rows        ; Graph                   -> number
           graph-number-of-cols        ; Graph                   -> number
           graph-position-ref          ; Graph Position          -> list
           graph-position-add!         ; Graph Position Position -> void
           graph-position-remove!      ; Graph Position Position -> void
           
           make-table                  ;                            -> Table
           table?                      ; object                     -> boolean
           table-lookup                ; Table symbol symbol        -> object / #f
           table-insert!               ; Table symbol symbol object -> void
           
           make-flag                   ; symbol      -> Flag
           flag?                       ; object      -> boolean
           flag-ref                    ; Flag        -> symbol
           flag-set!                   ; Flag symbol -> void
           
           position-in-direction       ; Position Direction      -> Position
           direction-from-to           ; Position Position       -> Direction
           matrix-contains-position?   ; Matrix Position         -> boolean
           remove-from                 ; object list             -> list
           add-to                      ; object list             -> list
           enumerate-interval          ; number number           -> list
           make-round-robin            ; object [object ...]     -> function
           make-image-file             ; string string           -> image-file / #f
           character->image-file       ; character string symbol -> image-file / #f
           string-pad                  ; string number           -> string
           string-pad-right            ; string number           -> string
           split-strings               ; string string number    -> string
           objects-union               ; list list               -> list
           )
  
  ;;; Position ADT
  ;;;_______________________________________________________________________
  
  (define (make-position x y)
    (cons 'position (cons x y)))
  
  (define (position? object)
    (and (pair? object)
         (eq? (car object) 'position)))
  
  (define (position-x position)
    (cadr position))
  
  (define (position-y position)
    (cddr position))
  
  (define (position=? position1 position2)
    (and (= (position-x position1) (position-x position2))
         (= (position-y position1) (position-y position2))))
  
  (define (position-manhattan-distance position1 position2)
    (+ (abs (- (position-x position1) (position-x position2)))
       (abs (- (position-y position1) (position-y position2)))))
  
  ;;; Direction ADT
  ;;;______________________________________________________________________
  
  (define (make-direction delta-x delta-y)
    (cons 'direction (cons delta-x delta-y)))
  
  (define (direction? object)
    (and (pair? object)
         (eq? (car object) 'direction)))
  
  (define (direction-delta-x direction)
    (cadr direction))
  
  (define (direction-delta-y direction)
    (cddr direction))
  
  (define (direction=? direction1 direction2)
    (and 
     (= (direction-delta-x direction1) (direction-delta-x direction2))
     (= (direction-delta-y direction1) (direction-delta-y direction2))))
  
  (define (direction-opposite direction)
    (make-direction (- (direction-delta-x direction))
                    (- (direction-delta-y direction))))
  
  ;; direction constants follow the convention stated in
  ;; "Schrijfopdracht1_78887_V2.pdf" Section 2.1.1
  (define north (make-direction  0 -1))
  (define south (make-direction  0  1))
  (define east  (make-direction  1  0))
  (define west  (make-direction -1  0))
  
  ;;: Matrix ADT
  ;;;______________________________________________________________________
  
  (define (make-matrix number-of-rows number-of-cols . filler)
    (let ((the-matrix (make-vector number-of-rows)))
      (define (initialize index value)
        (if (< index number-of-rows)
            (begin
              (vector-set! the-matrix 
                           index 
                           (make-vector number-of-cols value))
              (initialize (+ index 1) value))))
      (if (null? filler)
          (initialize 0 0)
          (initialize 0 (car filler)))
      (cons 'matrix the-matrix)))
  
  (define (matrix? object)
    (and (pair? object)
         (eq? (car object) 'matrix)))
  
  (define (matrix-number-of-rows matrix)
    (vector-length (cdr matrix)))
  
  (define (matrix-number-of-cols matrix)
    (vector-length (vector-ref (cdr matrix) 0)))
  
  (define (matrix-ref matrix row col)
    (let ((number-of-rows (matrix-number-of-rows matrix))
          (number-of-cols (matrix-number-of-cols matrix)))
      (if (< row number-of-rows)
          (if (< col number-of-cols)
              (vector-ref (vector-ref (cdr matrix) row) col)
              (error "matrix-ref : col out of range" col))
          (error "matrix-ref : row out of range" row))))
  
  (define (matrix-set! matrix row col object)
    (let ((number-of-rows (matrix-number-of-rows matrix))
          (number-of-cols (matrix-number-of-cols matrix)))
      (if (< row number-of-rows)
          (if (< col number-of-cols)
              (vector-set! (vector-ref (cdr matrix) row) col object)
              (error "matrix-set! : col out of range" col))
          (error "matrix-set! : row out of range" row))))    
  
  ;;** row = y, col = x !
  (define (matrix-position-ref matrix position)
    (matrix-ref matrix (position-y position) (position-x position)))
  
  ;;** row = y, col = x !
  (define (matrix-position-set! matrix position object)
    (matrix-set! matrix 
                 (position-y position) 
                 (position-x position) 
                 object))
  
  (define (matrix-for-each matrix procedure)
    (let ((last-row (- (matrix-number-of-rows matrix) 1))
          (last-col (- (matrix-number-of-cols matrix) 1)))
      (for-each
       (lambda (row)
         (for-each
          (lambda (col)
            (procedure matrix row col))
          (enumerate-interval 0 last-col)))
       (enumerate-interval 0 last-row))))
  
  ;;** by lack of a better name :-(
  (define (matrix-row-list matrix row proc lst)
    (define (traverse l col max-col)
      (if (< col max-col)
          (if (not (null? l))
              (begin
                (proc matrix row col (car l))
                (traverse (cdr l) (+ col 1) max-col)))))
    (let ((rows (matrix-number-of-rows matrix))
          (cols (matrix-number-of-cols matrix)))
      (if (< row rows)
          (traverse lst 0 cols))))
  
  (define (matrix->list matrix)
    (let ((items '()))
      (matrix-for-each 
       matrix 
       (lambda (mat row col)
         (set! items (cons (matrix-ref mat row col) items))))
      (reverse items)))
  
  ;;; Graph ADT
  ;;;______________________________________________________________________
  
  (define (make-graph number-of-rows number-of-cols)
    (let ((the-graph (make-matrix number-of-rows number-of-cols '())))
      (cons 'graph the-graph)))
  
  (define (graph? object)
    (and (pair? object)
         (eq? (car object) 'graph)))
  
  (define (graph-number-of-rows graph)
    (let ((matrix (cdr graph)))
      (matrix-number-of-rows matrix)))
  
  (define (graph-number-of-cols graph)
    (let ((matrix (cdr graph)))
      (matrix-number-of-cols matrix)))
  
  (define (graph-position-ref graph position)
    (matrix-position-ref (cdr graph) position))
  
  (define (graph-position-add! graph position adjacent-position)
    (let ((matrix (cdr graph)))
      (matrix-position-set!
       matrix
       position
       (add-to adjacent-position 
               (matrix-position-ref matrix position)))))
  
  (define (graph-position-remove! graph position adjacent-position)
    (let ((matrix (cdr graph)))
      (matrix-position-set!
       matrix
       position
       (remove-from adjacent-position 
                    (matrix-position-ref matrix position)))))
  
  ;;; Table ADT
  ;;;______________________________________________________________________
  
  (define (make-table) (list 'table))
  
  (define (table? object)
    (and (pair? object)
         (eq? (car object) 'table)))
  
  (define (table-lookup table key1 key2)
    (let ((subtable (assoc key1 (cdr table))))
      (if subtable
          (let ((record (assoc key2 (cdr subtable))))
            (if record
                (cdr record)
                #f))
          #f)))
  
  (define (table-insert! table key1 key2 value)
    (let ((subtable (assoc key1 (cdr table))))
      (if subtable
          (let ((record (assoc key2 (cdr subtable))))
            (if record
                (set-cdr! record value)
                (set-cdr! subtable
                          (cons (cons key2 value)
                                (cdr subtable)))))
          (set-cdr! table
                    (cons (list key1 (cons key2 value))
                          (cdr table))))))
  
  ;;; Flag ADT
  ;;;______________________________________________________________________
  
  (define (make-flag state)
    (cons 'flag state))
  
  (define (flag? object)
    (and (pair? object) 
         (eq? (car object) 'flag)))
  
  (define (flag-ref flag)
    (cdr flag))
  
  (define (flag-set! flag new-state)
    (set-cdr! flag new-state))
  
  ;;; General purpose functions
  ;;;______________________________________________________________________
  
  (define (position-in-direction position direction)
    (let ((x (position-x position))
          (y (position-y position))
          (delta-x (direction-delta-x direction))
          (delta-y (direction-delta-y direction)))
      (make-position (+ x delta-x) (+ y delta-y))))
  
  (define (direction-from-to position1 position2)
    (let ((x1 (position-x position1))
          (y1 (position-y position1))
          (x2 (position-x position2))
          (y2 (position-y position2)))
      (make-direction (- x2 x1) (- y2 y1))))
  
  (define (matrix-contains-position? matrix position)
    (let ((x (position-x position))
          (y (position-y position))
          (last-row (- (matrix-number-of-rows matrix) 1))
          (last-col (- (matrix-number-of-cols matrix) 1)))
      (and (>= x 0) (<= x last-col)
           (>= y 0) (<= y last-row))))
  
  (define (remove-from object lijst)
    (cond ((null? lijst) '())
          ((not (member object lijst)) lijst)
          ((equal? (car lijst) object) (cdr lijst))
          (else (cons (car lijst) (remove-from object (cdr lijst))))))
  
  (define (add-to object lijst)
    (if (member object lijst)
        lijst
        (cons object lijst)))
  
  (define (enumerate-interval from to)
    (if (> from to)
        '()
        (cons from (enumerate-interval (+ from 1) to))))
  
  (define (make-round-robin . args)
    (if (null? args)
        (error "MAKE-ROUND-ROBIN -- expects at least 1 argument")
        (let ((lijst args))
          (lambda ()
            (if (null? lijst) (set! lijst args))
            (let ((next-item (car lijst)))
              (set! lijst (cdr lijst))
              next-item)))))
  
  (define (make-image-file directory-name file-name)
    (let ((image-file (string-append directory-name "/" file-name)))
      (if (file-exists? image-file)
          image-file
          #f)))
  
  (define (character->image-file character directory-name file-type)
    (let ((file-name 
           (string-append 
            (list->string (list character)) "." (symbol->string file-type))))
      (make-image-file directory-name file-name)))
  
  (define (split-strings string1 string2 len)
    (let ((len2 (- len (string-length string1))))
      (if (> len2 0)
          (string-append string1 (string-pad string2 len2))
          (string-pad string2 len))))
  
  (define (objects-union objects1 objects2)
    (cond ((null? objects1) 
           objects2)
          ((memq (car objects1) objects2) 
           (objects-union (cdr objects1) objects2))
          (else 
           (objects-union (cdr objects1) (cons (car objects1) objects2)))))
)
    