;;;
;;; pacman-gui.ss
;;;
;;; Jan Cornelis
;;; 2007-2008
;;;
;;; This module is used to display stuff on the screen.
;;; Herefore it provides the following procedures:
;;;
;;; initialize-gui
;;; --------------
;;; This function has to be called before anything else. Basically it will
;;; use the given dc% object and the given dimensions (number of rows and
;;; number of columns) to divide the imaginary plane in a number of square
;;; cells. 
;;;
;;; refresh-gui
;;; -----------
;;; This function will be called when the GUI is resized. It can also be
;;; called to force the screen to be refreshed.
;;;
;;; draw-nothing
;;; ------------
;;; Draws a rectangle in the background color in the cell identified by the 
;;; given Position.
;;;
;;; draw-wall
;;; ---------
;;; Draws a Wall in the cell identified by the given Position. A given
;;; list of Directions is used to determine the shape of the Wall. 
;;; Basically if a Direction is present, then the neighbouring cell in 
;;; this Direction is also a Wall.
;;;
;;; draw-pacman
;;; -----------
;;; Draws a Pacman in the cell identified by the given Position. A given
;;; Direction is used to draw Pacman's mouth.
;;;
;;; draw-ghost
;;; ----------
;;; Draws a ghost in the cell identified by the given Position. A number
;;; of given parameters are used to determine the look of the Ghost: it's
;;; Direction, it's color and it's state.
;;;
;;; draw-image
;;; ----------
;;; Draws an image contained in a file identified by a given file name
;;; in the cell identified by the given Position. This procedure is used
;;; to draw the rewards, as well as characters and digits. It uses an
;;; internal cache to avoid the repetitive opening of the same files.
;;; The caller of draw-image is trusted to pass a file-name of an existing
;;; file.
;;;
;;; The provided procedures use an internal "object" to do their work.
;;; The object is created when initialize-gui is called
;;;________________________________________________________________________

(module pacman-gui mzscheme
  
  (require (lib "mred-sig.ss" "mred")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "math.ss")
           (lib "pregexp.ss")
           "pacman-util.ss"
           "pacman-glob.ss")
  
  (provide initialize-gui ; dc% number number                -> void
           refresh-gui    ;                                  -> void
           draw-nothing   ; Position                         -> void
           draw-wall      ; Position                         -> void
           draw-pacman    ; Position Direction               -> void
           draw-ghost     ; Position Direction string symbol -> void
           draw-image)    ; Position string                  -> void
  
  ;;; Module global variables
  ;;;______________________________________________________________________
  
  (define *gui* #f)
  
  (define *background-brush*  
    (make-object brush% BACKGROUND-COLOR 'solid)) 
  (define *background-pen*    
    (make-object pen% BACKGROUND-COLOR 1 'transparent))  
  (define *wall-brush*        
    (make-object brush% WALL-COLOR 'solid))  
  (define *wall-pen*          
    (make-object pen% WALL-COLOR 1 'transparent))  
  (define *pacman-brush*      
    (make-object brush% PACMAN-COLOR 'solid))  
  (define *pacman-pen*        
    (make-object pen% PACMAN-COLOR 1 'transparent))  
  (define *weak-ghost-brush*  
    (make-object brush% WEAK-GHOST-COLOR 'solid))  
  (define *weak-ghost-pen*    
    (make-object pen% WEAK-GHOST-COLOR 1 'transparent))  
  (define *ghost-mouth-brush* 
    (make-object brush% GHOST-MOUTH-COLOR 'solid))  
  (define *ghost-mouth-pen*   
    (make-object pen% GHOST-MOUTH-COLOR 1 'transparent)) 
  (define *eyewhite-brush*    
    (make-object brush% GHOST-EYEWHITE-COLOR 'solid))  
  (define *eyewhite-pen*      
    (make-object pen% GHOST-EYEWHITE-COLOR 1 'transparent))  
  (define *eyepupil-brush*    
    (make-object brush% GHOST-EYEPUPIL-COLOR 'solid)) 
  (define *eyepupil-pen*      
    (make-object pen% GHOST-EYEPUPIL-COLOR 1 'transparent))
  
  ;;; Module interface
  ;;;______________________________________________________________________
  
  (define (initialize-gui dc number-of-rows number-of-cols)
    (set! *gui* (make-gui dc number-of-rows number-of-cols)))
  
  (define (refresh-gui)
    (if *gui* (*gui* 'refresh)))  
  
  (define (draw-nothing position)
    (if *gui* (*gui* 'draw-nothing position)))
  
  (define (draw-wall position shape)
    (if *gui* (*gui* 'draw-wall position shape)))
  
  (define (draw-pacman position direction)
    (if *gui* (*gui* 'draw-pacman position direction)))
  
  (define (draw-ghost position direction color-name state)
    (if *gui* (*gui* 'draw-ghost position direction color-name state)))
  
  (define (draw-image position file-name)
    (if *gui* (*gui* 'draw-image position file-name)))
  
  ;;; Internal GUI ADT
  ;;;
  ;;; Messages:
  ;;;
  ;;;  'refresh      :                                  -> void
  ;;;  'draw-nothing : Position                         -> void
  ;;;  'draw-wall    : Position list                    -> void
  ;;;  'draw-pacman  : Postion Direction                -> void
  ;;;  'draw-ghost   : Position Direction string symbol -> void
  ;;;  'draw-image   : Position Image-File              -> void
  ;;;______________________________________________________________________
  (define (make-gui dc number-of-rows number-of-cols) 
    (let ((the-cache (make-matrix number-of-rows number-of-cols))
          (cell-size 1))
      ;;
      ;; -- initialize
      ;; When the object is first created or the gui is resized, 
      ;; geometrical settings are recalculated and the screen is cleared.
      ;;
      (define (initialize)
        (let-values (((w h) (send dc get-size)))
          (set! cell-size (min (/ h number-of-rows) (/ w number-of-cols)))
          (send dc clear)
          (send dc set-brush *background-brush*)
          (send dc set-pen *background-pen*)
          (send dc set-scale 1 1)
          (send dc set-origin 0 0)
          (send dc draw-rectangle 0 0 w h)))
      ;;
      ;; -- wrap-function
      ;; In order to redraw the current contents of the gui when it is
      ;; resized every call to a draw procedure is stored in the-cache
      ;; before it is actually executed.
      ;;
      (define (wrap-function fun pos . args)
        (matrix-position-set! the-cache pos (cons fun (cons pos args)))
        (apply fun (cons dc (cons cell-size (cons pos args)))))
      ;;
      ;; -- refresh
      ;; when the gui is resized, it needs to be re-initialized and the
      ;; current contents need to be redrawn using the-cache.
      ;;
      (define (refresh)
        (initialize)
        (matrix-for-each
         the-cache
         (lambda (matrix row col)
           (let ((lijst (matrix-ref matrix row col)))
             (if (list? lijst)
                 (apply (car lijst) (cons dc (cons cell-size (cdr lijst)))))))))
      ;;
      ;; -- interface functions
      ;;
      (define (draw-nothing position)
        (wrap-function _draw-nothing position))
      (define (draw-wall position directions)
        (wrap-function _draw-wall position directions))
      (define (draw-pacman position direction)
        (wrap-function _draw-pacman position direction))
      (define (draw-ghost position direction color-name state)
        (wrap-function _draw-ghost position direction color-name state))
      (define (draw-image position file-name)
        (wrap-function _draw-image position file-name))
      ;;
      ;; -- dispatch
      ;;
      (define (dispatch m . args)
        (cond
          ((eq? m 'refresh)      (apply refresh args))          
          ((eq? m 'draw-nothing) (apply draw-nothing args))
          ((eq? m 'draw-wall)    (apply draw-wall args))
          ((eq? m 'draw-pacman)  (apply draw-pacman args))
          ((eq? m 'draw-ghost)   (apply draw-ghost args))
          ((eq? m 'draw-image)   (apply draw-image args))
          (else (error "PACMAN-GUI: unknown message" m))))
      dispatch))
  
  ;;; drawing procedures
  ;;;______________________________________________________________________
  
  ;;; _draw-path : dc% number -> void
  ;;; draws a rectangle on the dc% object
  ;;;______________________________________________________________________
  (define (_draw-nothing dc size pos)
    (project-context dc size pos)
    (send dc set-brush *background-brush*)
    (send dc set-pen *background-pen*)
    (send dc draw-rectangle 0 0 1 1))
  
  ;;; _draw-wall : dc% number Position list -> void
  ;;; draws a wall on the dc% object
  ;;;______________________________________________________________________
  (define (_draw-wall dc size pos dirs)
    (define sub-size (/ 1 3))
    (project-context dc size pos)
    (send dc set-brush *wall-brush*)
    (send dc set-pen *wall-pen*)
    (send dc draw-rectangle sub-size sub-size sub-size sub-size)
    ;;
    ;; Fill up the sub-cell for every Direction in dirs. Such a Direction 
    ;; indicates that the cell neighbouring in Direction is also a Wall.
    ;;
    (for-each
     (lambda (d)
       (let* ((delta-x (direction-delta-x d))
              (delta-y (direction-delta-y d))
              (x (* sub-size (+ delta-x 1)))
              (y (* sub-size (+ delta-y 1))))
         (send dc draw-rectangle x y sub-size sub-size)))
     dirs))
  
  ;;; _draw-pacman : dc% number Position Direction -> void
  ;;; draws a pacman on the dc% object
  ;;;______________________________________________________________________
  (define (_draw-pacman dc size pos dir)
;    (_draw-ghost dc size pos dir "Yellow" 'strong))
    (project-context dc size pos)
    (send dc set-brush *pacman-brush*)
    (send dc set-pen *pacman-pen*)
    (send dc draw-path (make-pacman-contour dir)))
  
  ;;; _draw-ghost : dc% number Position Direction string symbol -> void
  ;;; draws a ghost on the dc% object
  ;;;______________________________________________________________________
  (define (_draw-ghost dc size pos dir color-name state)
    (project-context dc size pos)
    (if (eq? state 'strong)   ; just draw a contour
        (begin
          (send dc set-brush color-name 'solid)
          (send dc draw-path (make-ghost-contour))))
    (if (eq? state 'weak)     ; draw a contour and a mouth
        (begin
          (send dc set-brush *weak-ghost-brush*)
          (send dc set-pen *weak-ghost-pen*)
          (send dc draw-path (make-ghost-contour))
          (send dc set-brush *ghost-mouth-brush*)
          (send dc set-pen *ghost-mouth-pen*)
          (send dc draw-path (make-ghost-mouth))))
    (draw-ghost-eyes dc dir)) ; always draw the eyes

  ;;; _draw-image : dc% number Position string -> void
  ;;; draws a bitmap on the dc% object using a file-name
  ;;;______________________________________________________________________  
  (define (_draw-image dc size pos file-name)
    (let* ((bitmap (get-bitmap file-name)) ; lookup in a bitmap cache
           (w (send bitmap get-width))
           (h (send bitmap get-height)))
      (project-context dc size pos (/ 1 w) (/ 1 h))
      (send dc draw-bitmap bitmap 0 0)))
  
  ;;; Helper functions
  ;;;______________________________________________________________________

  ;;; project-context : dc% number Position [number number] -> void
  ;;; manipulates the dc% object in such a way that the cell at position
  ;;;  - appears to originate in (0 . 0)
  ;;;  - appears to have height and width 1 or scale-x and scale-y
  ;;; the optional scaling arguments are used when drawing bitmaps
  ;;;______________________________________________________________________
  (define (project-context dc size pos . scale)
    (let ((x (position-x pos))
          (y (position-y pos))
          (scale-x (if (null? scale) 1 (car scale)))
          (scale-y (if (null? scale) 1 (cadr scale))))
      (send dc set-origin (* x size) (* y size))
      (send dc set-scale (* scale-x size) (* scale-y size))))
  
  ;;; make-pacman-contour : Direction -> dc-path%
  ;;;______________________________________________________________________
  (define (make-pacman-contour dir)
    (let* ((path (make-object dc-path%))
           (angles (direction->angles dir))
           (from-angle (car angles))
           (to-angle   (cdr angles)))
      (send path arc 0 0 1 1 from-angle to-angle)
      (send path line-to 0.5 0.5)
      path))
  
  ;;; direction->angles : Direction -> pair
  ;;;______________________________________________________________________
  (define (direction->angles dir)
    (cond
      ((direction=? dir east)  (cons (* 0.125 pi) (* -0.125 pi)))
      ((direction=? dir north) (cons (* 0.625 pi) (*  0.375 pi)))
      ((direction=? dir west)  (cons (* 1.125 pi) (*  0.875 pi)))
      ((direction=? dir south) (cons (* 1.625 pi) (*  1.375 pi)))
      (else (error "PACMAN-GUI: unknown direction" dir))))
  
  
  ;;; make-ghost-contour : -> dc-path%
  ;;;______________________________________________________________________
  (define (make-ghost-contour)
    (let ((path (make-object dc-path%)))
      (zigzag-path path 0 1 1 0.125 -0.125)
      (send path curve-to 0.833 0.200 0.667 0.033 0.500 0.000)
      (send path curve-to 0.333 0.033 0.167 0.200 0.000 1.000)
      path))
  
  ;;; make-ghost-mouth : -> dc-path%
  ;;;______________________________________________________________________
  (define (make-ghost-mouth)
    (let ((path (make-object dc-path%)))
      (zigzag-path path 0.250 0.800 0.750 0.125 -0.125)
      (send path line-to 0.750 0.750)
      (zigzag-path path 0.750 0.750 0.250 -0.125 -0.125)
      (send path line-to 0.250 0.800)
      path))
  
  ;;; draw-ghost-eyes : dc% Direction -> void
  ;;; the position of the eyepits indicates the ghost's direction
  ;;;______________________________________________________________________
  (define (draw-ghost-eyes dc direction)
    (let* ((dx (direction-delta-x direction))
           (dy (direction-delta-y direction))
           (R  0.08)
           (X1 0.35)
           (X2 0.65)
           (Y  0.35)
           (r  0.02)
           (x1 (+ X1 (* dx (- R (* 2 r)))))
           (x2 (+ X2 (* dx (- R (* 2 r)))))
           (y  (+ Y  (* dy (- R (* 2 r))))))
      (send dc set-brush *eyewhite-brush*)
      (send dc set-pen *eyewhite-pen*)
      (draw-circle dc X1 Y R)
      (draw-circle dc X2 Y R)
      (send dc set-brush *eyepupil-brush*)
      (send dc set-pen *eyepupil-pen*)
      (draw-circle dc x1 y r)
      (draw-circle dc x2 y r)))
  
  ;;; zigzag-path : dc-path% number number number number number -> void
  ;;; ** The caller of this function should make sure that:
  ;;;    (= (remainder (- to-x from-x) x-step) 0) 
  ;;;______________________________________________________________________
  (define (zigzag-path p from-x from-y to-x x-step y-step)
    (define (continue x y op)
      (if (not (= x to-x))
          (let ((new-op (if (eq? op +) - +)))
            (send p line-to (+ x x-step) (op y y-step))
            (continue (+ x x-step) (op y y-step) new-op))))
    (send p move-to from-x from-y)
    (continue from-x from-y +)) 
  
  ;;; draw-circle : dc% number number number -> void
  ;;;______________________________________________________________________
  (define (draw-circle dc x y r)
    (send dc draw-ellipse (- x r) (- y r) (* 2 r) (* 2 r)))
  
  ;;; Bitmap-cache
  ;;; This is the bitmap cache mentionned higher. The bitmap-cache is just
  ;;; a list of file-name, bitmap pairs that is "indexed" using assoc. 
  ;;; Because the list is expected to be small, this is efficient enough.
  ;;;______________________________________________________________________
  
  (define (make-bitmap-cache)
    (let ((cached-bitmaps '()))
      (lambda (file-name)
        (let ((pair (assoc file-name cached-bitmaps)))
          (if pair
              (cdr pair)
              (let* ((file-type (file-name->file-type file-name))
                     (bitmap (make-object bitmap% file-name file-type)))
                ;;
                ;; The entry was not found, create it now
                ;;
                (set! cached-bitmaps (cons (cons file-name bitmap)
                                           cached-bitmaps))
                bitmap))))))
  
  (define get-bitmap (make-bitmap-cache))
  
  ;;; file-name->file-type : string -> symbol
  ;;; This function will determine the file-type based upon the extension
  ;;; of it's filename. A regular expression is used to split the file-name
  ;;;______________________________________________________________________
  (define (file-name->file-type file-name)
    (let* ((parts (pregexp-split (pregexp-quote ".") file-name)))
      (string->symbol 
       (string-downcase (cadr parts)))))
  
)