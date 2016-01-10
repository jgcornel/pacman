;;;
;;; pacman-maze.ss
;;; 
;;; Jan Cornelis
;;; 2007-2008
;;;
;;; This module lies in the very heart of the Pacman project. It provides
;;; all ADT's to represent the well known parts of the Pacman game:
;;;
;;; Maze
;;; ----
;;; A Maze represents the Pacman gameboard. A Maze is basically a matrix
;;; containing the cells of the Pacman gameboard. The cells are one of
;;; three types: Display, Path or Wall. For quick access some elements can 
;;; be accessed directly: Pacman, a list of all Ghosts and a Position for 
;;; the Fruit Reward. 
;;;
;;; Display
;;; -------
;;; A Display is a cell that contains a character. It is used to display
;;; the score and the level. For the purpose of this project it is assumed
;;; that if there is a number of Display cells, they will be found in the
;;; upper row of the Maze.
;;;
;;; Path
;;; ----
;;; A Path is a cell that can contain 0 or 1 Reward, 0 or 1 Pacman and 0 or
;;; more Ghosts. It is possible to add or remove any of these possible
;;; components.
;;;
;;; Wall
;;; ----
;;; A Wall is a cell through which a wall runs. The Wall's shape is a list
;;; of Directions. The presence of a Direction in this list, means that the
;;; neighbouring cell in this Direction is also a Wall.
;;;
;;; Reward
;;; ------
;;; A Reward is a passive element of the Pacman gameboard. A Reward is a
;;; very generic ADT and can be used to create Cookies, Energizers, Fruits
;;; and the Home of a Ghost. Associated with a Reward are a type, a score
;;; and a file-name. The file-name identifies an image file that should be
;;; used to display the Reward.
;;;
;;; Pacman
;;; ------
;;; A Pacman represents the hero of our game: Pacman. The Pacman ADT offers
;;; the necessary methods needed to manipulate Pacman in the context of the 
;;; game. 
;;;
;;; Ghost
;;; -----
;;; A Ghost ADT represents the bad boy of our game: a ghost. The Ghost ADT
;;; offers the necessary methods needed to manipulate a Ghost in the
;;; context of the game. It is interesting to note that every Ghost has
;;; an associated function that will determine how it moves. This function
;;; might or might not be intelligent.
;;;
;;; For the moment Pacman and Ghost are separate ADTs. Probably it would be
;;; better to make one Agent ADT and to use this ADT to create the Pacman
;;; and Ghost ADTs. If time allows this might be done.
;;;
;;; It should be noted that every ADT offers a 'display method and that
;;; every ADT, except the Maze ADT offers by default the methods
;;; 'get-position and 'get-type.
;;;
;;; A detailed overview of each ADT's interface is given in it's header.
;;;________________________________________________________________________

(module pacman-maze mzscheme
  
  (require "pacman-util.ss"
           "pacman-glob.ss"
           "pacman-gui.ss")
  
  (provide make-maze
           make-display
           make-path
           make-wall
           make-reward
           make-pacman
           make-ghost)
  
  ;;; Maze ADT
  ;;;
  ;;; Messages:
  ;;;
  ;;;  'get                :          -> Cell
  ;;;  'set!               : Cell     -> void
  ;;;  'for-each           :          -> void
  ;;;  'contains-position? : Position -> boolean
  ;;;  'to-list            :          -> list
  ;;;  'number-of-rows     :          -> number
  ;;;  'number-of-cols     :          -> number
  ;;;  'get-the-pacman     :          -> Pacman
  ;;;  'set-the-pacman!    : Pacman   -> void
  ;;;  'get-the-ghosts     :          -> list
  ;;;  'add-ghost          : Ghost    -> void
  ;;;  'display            :          -> void
  ;;;  'to-graph           :          -> void
  ;;;  'set-string!        : string   -> void
  ;;;______________________________________________________________________
  (define (make-maze number-of-rows number-of-cols)
    (let ((the-maze (make-matrix number-of-rows number-of-cols))
          (the-pacman #f)
          (the-ghosts '())
          (fruit-position #f))
      ;;
      ;; getter and setters
      ;;
      (define (get position)
        (matrix-position-ref the-maze position))
      (define (_set! position cell)
        (matrix-position-set! the-maze position cell))
      (define (_for-each proc)
        (matrix-for-each
         the-maze
         (lambda (matrix row col)
           (let ((cell (matrix-ref matrix row col)))
             (proc cell)))))
      (define (contains-position? position) 
        (matrix-contains-position? the-maze position))
      (define (to-list)
        (matrix->list the-maze))
      (define (get-number-of-rows) 
        number-of-rows)
      (define (get-number-of-cols) 
        number-of-cols)
      (define (get-the-pacman) 
        the-pacman)
      (define (set-the-pacman! pacman)
        (set! the-pacman pacman))
      (define (get-the-ghosts) 
        the-ghosts)
      (define (add-ghost ghost)
        (set! the-ghosts (add-to ghost the-ghosts)))
      (define (get-fruit-position)
        fruit-position)
      (define (set-fruit-position! new-fruit-position)
        (set! fruit-position new-fruit-position))
      ;;
      ;; _display: calls 'display for every cell
      ;;
      (define (_display)
        (_for-each
         (lambda (cell)
           (cell 'display))))
      ;;
      ;; to-graph: returns a Graph representation
      ;;
      (define (to-graph)
        (let ((graph (make-graph number-of-rows number-of-cols)))
          (define (filler)
            (_for-each
             (lambda (cell)
               (let* ((position (cell 'get-position))
                      (type (cell 'get-type)))
                 (if (eq? type 'path)
                     (let ((neighbour-positions
                            (map 
                             (lambda (d) 
                               (position-in-direction position d))
                             (list east west north south))))
                       (for-each
                        (lambda (np)
                          (if (contains-position? np)
                              (let* ((cell (get np))
                                     (type (cell 'get-type)))
                                (if (eq? type 'path)
                                    (graph-position-add! graph 
                                                         position 
                                                         np)))))
                        neighbour-positions)))))))
          (filler)
          graph))
      ;;
      ;; set-string!: uses a string to manipulate the display row
      ;;
      (define (set-string! string row)
        (matrix-row-list
         the-maze
         row
         (lambda (m r c v)
           (let* ((cell (matrix-ref m r c))
                  (type (cell 'get-type)))
             (if (eq? type 'display)
                 (cell 'set-character! v))))
         (string->list string)))
      ;;
      ;; dispatch
      ;;
      (define (dispatch m . args)
        (cond
          ((eq? m 'get)                 (apply get args))
          ((eq? m 'set!)                (apply _set! args))
          ((eq? m 'for-each)            (apply _for-each args))
          ((eq? m 'contains-position?)  (apply contains-position? args))
          ((eq? m 'to-list)             (apply to-list args))
          ((eq? m 'number-of-rows)      (apply get-number-of-rows args))
          ((eq? m 'number-of-cols)      (apply get-number-of-cols args))
          ((eq? m 'get-the-pacman)      (apply get-the-pacman args))
          ((eq? m 'set-the-pacman!)     (apply set-the-pacman! args))
          ((eq? m 'get-the-ghosts)      (apply get-the-ghosts args))
          ((eq? m 'add-ghost)           (apply add-ghost args))
          ((eq? m 'get-fruit-position)  (apply get-fruit-position args))
          ((eq? m 'set-fruit-position!) (apply set-fruit-position! args))
          ((eq? m 'display)             (apply _display args))
          ((eq? m 'to-graph)            (apply to-graph args))
          ((eq? m 'set-string!)         (apply set-string! args))
          (else (error "PACMAN-MAZE -- unknown message:" m))))
      dispatch))
  
  ;;; Display ADT
  ;;;
  ;;; Messages:
  ;;;
  ;;;  'get-position   :           -> Position
  ;;;  'get-type       :           -> symbol
  ;;;  'get-character  :           -> character
  ;;;  'set-character! : character -> void
  ;;;  'display        :           -> void
  ;;;______________________________________________________________________
  (define (make-display position character)
    ;;
    ;; getters and setters
    ;;
    (define (get-position)
      position)
    (define (get-type)
      'display)
    (define (get-character)
      character)
    (define (set-character! new-character)
      (set! character new-character))
    ;;
    ;; _display : 
    ;;
    (define (_display)
      (let ((image-file 
             (character->image-file character IMAGE-DIRECTORY IMAGE-TYPE)))
        (if image-file
            (draw-image position image-file)
            (draw-nothing position))))
    ;;
    ;; dispatch
    ;;
    (define (dispatch m . args)
      (cond
        ((eq? m 'get-position)   (apply get-position args))
        ((eq? m 'get-type)       (apply get-type args))
        ((eq? m 'get-character)  (apply get-character args))
        ((eq? m 'set-character!) (apply set-character! args))
        ((eq? m 'display)        (apply _display args))
        (else (error "PACMAN-DISPLAY -- unknown message:" m))))
    dispatch)
  
  ;;; Path ADT
  ;;;
  ;;; Messages:
  ;;;
  ;;;  'get-position :        -> Position
  ;;;  'get-type     :        -> symbol
  ;;;  'get-reward   :        -> Reward
  ;;;  'set-reward!  : Reward -> void
  ;;;  'get-pacman   :        -> Pacman
  ;;;  'set-pacman!  : Pacman -> void
  ;;;  'get-ghosts   :        -> list
  ;;;  'add-ghost    : Ghost  -> void
  ;;;  'remove-ghost : Ghost  -> void
  ;;;  'display      :        -> void
  ;;;______________________________________________________________________
  (define (make-path position reward pacman ghosts)
    ;;
    ;; getters and setters
    ;;
    (define (get-position) 
      position)
    (define (get-type) 
      'path)
    (define (get-reward) 
      reward)
    (define (set-reward! a-reward) 
      (set! reward a-reward))
    (define (get-pacman) 
      pacman)
    (define (set-pacman! a-pacman) 
      (set! pacman a-pacman))
    (define (get-ghosts) 
      ghosts)
    (define (add-ghost a-ghost) 
      (set! ghosts (add-to a-ghost ghosts)))
    (define (rem-ghost a-ghost) 
      (set! ghosts (remove-from a-ghost ghosts)))
    ;;
    ;; _display
    ;;
    (define (_display)
      (let ((agents (if pacman (cons pacman ghosts) ghosts)))
        (if (and (null? agents) (not reward))
            (draw-nothing position)
            (begin
              (if reward (reward 'display))
              (for-each
               (lambda (a)
                 (a 'display)) agents)))))
    ;;
    ;; dispatch
    ;;
    (define (dispatch m . args)
      (cond
        ((eq? m 'get-position) (apply get-position args))
        ((eq? m 'get-type)     (apply get-type args))
        ((eq? m 'get-reward)   (apply get-reward args))
        ((eq? m 'set-reward!)  (apply set-reward! args))
        ((eq? m 'get-pacman)   (apply get-pacman args))
        ((eq? m 'set-pacman!)  (apply set-pacman! args))
        ((eq? m 'get-ghosts)   (apply get-ghosts args))
        ((eq? m 'add-ghost)    (apply add-ghost args))
        ((eq? m 'remove-ghost) (apply rem-ghost args))
        ((eq? m 'display)      (apply _display args))
        (else (error "PACMAN-PATH -- unknown message:" m))))   
    dispatch)
  
  ;;; Wall ADT
  ;;;
  ;;; Messages
  ;;;
  ;;;  'get-position  :           -> Position
  ;;;  'get-type      :           -> symbol
  ;;;  'add-direction : Direction -> void
  ;;;  'display       :           -> void
  ;;;______________________________________________________________________
  (define (make-wall position shape)
    ;;
    ;; getters and setters
    ;;
    (define (get-position) 
      position)
    (define (get-type) 
      'wall)
    (define (add-direction direction) 
      (set! shape (add-to direction shape)))
    ;;
    ;; _display
    ;;
    (define (_display) (draw-wall position shape))
    ;;
    ;; dispatch
    ;;
    (define (dispatch m . args)
      (cond
        ((eq? m 'get-position)  (apply get-position args))
        ((eq? m 'get-type)      (apply get-type args))
        ((eq? m 'add-direction) (apply add-direction args))
        ((eq? m 'display)       (apply _display args))
        (else (error "PACMAN-WALL -- unknown message:" m))))
    dispatch)
  
  ;;; Reward ADT
  ;;;
  ;;; Messages:
  ;;;
  ;;;  'get-position : -> Position
  ;;;  'get-type     : -> symbol
  ;;;  'get-score    : -> number
  ;;;  'display      : -> void
  ;;;______________________________________________________________________
  (define (make-reward position type file-name score)
    ;;
    ;; getters and setters
    ;;
    (define (get-position) 
      position)
    (define (get-type) 
      type)
    (define (get-score) 
      score)
    ;;
    ;; _display
    ;;
    (define (_display)
      ;
      ; Construct the full-name of the corresponding file
      ; and use it if it exists
      ;
      (let ((file-path
             (string-append IMAGE-DIRECTORY file-name)))
        (if (file-exists? file-path)
            (draw-image position file-path)
            (draw-nothing position))))
    ;;
    ;; dispatch
    ;;
    (define (dispatch m . args)
      (cond
        ((eq? m 'get-position) (apply get-position args))
        ((eq? m 'get-type)     (apply get-type args))
        ((eq? m 'get-score)    (apply get-score args))
        ((eq? m 'display)      (apply _display args))
        (else (error "PACMAN-REWARD -- unknown message:" m))))
    dispatch)
  
  ;;; Pacman ADT
  ;;;
  ;;; Messages:
  ;;;
  ;;;  'get-position   :           -> Position
  ;;;  'get-type       :           -> symbol
  ;;;  'get-maze       :           -> Maze
  ;;;  'get-direction  :           -> Direction
  ;;;  'set-direction! : Direction -> void
  ;;;  'get-lives      :           -> number
  ;;;  'decr-lives     :           -> void
  ;;;  'incr-lives     :           -> void
  ;;;  'get-score      :           -> number
  ;;;  'add-points     :           -> void
  ;;;  'get-state      :           -> symbol
  ;;;  'set-state!     : symbol    -> void
  ;;;  'display        :           -> void
  ;;;  'go-home        :           -> void
  ;;;  'move           :           -> void
  ;;;______________________________________________________________________
  (define (make-pacman home direction maze lives)
    (let ((position home)
          (score 0)
          (state 'strong)
          (new-direction direction)) ;** improves the user's experience
      ;;
      ;; getters and setters
      ;;
      (define (get-position) 
        position)
      (define (get-type) 
        'pacman)
      (define (get-maze) 
        maze)
      (define (get-direction) 
        direction)
      (define (set-direction! a-direction) 
        (set! new-direction a-direction))  ;** do not change direction yet
      (define (get-lives) 
        lives)
      (define (decr-lives) 
        (set! lives (- lives 1))
        (if (= lives 0)
            (set! state 'dead)
            (set! state 'weak)))
      (define (incr-lives) 
        (set! lives (+ lives 1)))
      (define (get-score) 
        score)
      (define (add-points points) 
        (set! score (+ score points)))
      (define (get-state) 
        state)
      (define (set-state! a-state) 
        (set! state a-state))
      ;;
      ;; _display
      ;;
      (define (_display) 
        (draw-pacman position direction))
      ;;
      ;; go-home: used when restarting the current game
      ;;
      (define (go-home)
        (let ((curr-cell (maze 'get position))
              (home-cell (maze 'get home)))
          (set! position home)
          (set! direction PACMAN-DIRECTION)
          (curr-cell 'set-pacman! #f)
          (home-cell 'set-pacman! self)))
      ;;
      ;; move
      ;; It is only when Pacman moves that the current Direction is changed
      ;; This is only done when Pacman can actually move forward in the new
      ;; Direction. This way of doing things improves the user's experience
      ;; as he can pre-change Pacman's Direction
      ;;
      (define (move)
        (let* ((curr-cell (maze 'get position))
               (old-next-position 
                (position-in-direction position direction))
               (new-next-position 
                (position-in-direction position new-direction))
               (new-next-cell     
                (if (maze 'contains-position? new-next-position)
                    (maze 'get new-next-position)
                    #f))
               (old-next-cell
                (if (maze 'contains-position? old-next-position)
                    (maze 'get old-next-position)
                    #f))
               (new-next-type (if new-next-cell 
                                  (new-next-cell 'get-type) 
                                  #f))
               (old-next-type (if old-next-cell 
                                  (old-next-cell 'get-type) 
                                  #f)))
          (cond
            ((eq? new-next-type 'path)
             (set! position new-next-position)
             (set! direction new-direction)
             (curr-cell 'set-pacman! #f)
             (new-next-cell 'set-pacman! self))
            ((eq? old-next-type 'path)
             (set! position old-next-position)
             (curr-cell 'set-pacman! #f)
             (old-next-cell 'set-pacman! self)))))
      ;;
      ;; dispatch called self
      ;;
      (define (self m . args)
        (cond
          ((eq? m 'get-position)   (apply get-position args))
          ((eq? m 'get-type)       (apply get-type args))
          ((eq? m 'get-maze)       (apply get-maze args))
          ((eq? m 'get-direction)  (apply get-direction args))
          ((eq? m 'set-direction!) (apply set-direction! args))
          ((eq? m 'get-lives)      (apply get-lives args))
          ((eq? m 'decr-lives)     (apply decr-lives args))
          ((eq? m 'incr-lives)     (apply incr-lives args))
          ((eq? m 'get-score)      (apply get-score args))
          ((eq? m 'add-points)     (apply add-points args))
          ((eq? m 'get-state)      (apply get-state args))
          ((eq? m 'set-state!)     (apply set-state! args))
          ((eq? m 'display)        (apply _display args))
          ((eq? m 'go-home)        (apply go-home args))
          ((eq? m 'move)           (apply move args))
          (else (error "PACMAN-PACMAN -- unknown message:" m))))
      self))
  
  ;;; Ghost ADT
  ;;;
  ;;; Messages:
  ;;;
  ;;;  'get-position   :           -> Position
  ;;;  'get-type       :           -> symbol
  ;;;  'get-maze       :           -> Maze
  ;;;  'get-direction  :           -> Direction
  ;;;  'set-direction! : Direction -> void
  ;;;  'get-state      :           -> symbol
  ;;;  'set-state!     : symbol    -> void
  ;;;  'display        :           -> void
  ;;;  'move           :           -> void
  ;;;______________________________________________________________________
  (define (make-ghost home direction maze color-name ai-fun)
    (let ((position home)
          (state 'strong)
          ;;
          ;; graph cannot be constructed when the ghost is first created
          ;; because at that point in time maze is still under construction
          ;;
          (graph #f))
      ;;
      ;; getters and setters
      ;;
      (define (get-position) 
        position)
      (define (get-type) 
        'ghost)
      (define (get-maze) 
        maze)
      (define (get-direction) 
        direction)
      (define (set-direction! a-direction) 
        (set! direction a-direction))
      (define (get-state) 
        state)
      (define (set-state! a-state) 
        (set! state a-state))
      (define (set-ai-fun! a-fun) 
        (set! ai-fun a-fun))
      ;;
      ;; _display
      ;;
      (define (_display) (draw-ghost position direction color-name state))
      ;;
      ;; go-home: used when restarting the current game
      ;;      
      (define (go-home)
        (let* ((curr-cell (maze 'get position))
               (home-cell (maze 'get home)))
          (set! position home)
          (set! direction GHOST-DIRECTION)
          (curr-cell 'remove-ghost self)
          (home-cell 'add-ghost self)))
      ;;
      ;; move
      ;; When move is called for the first time graph is set! 
      ;; ai-fun is used to calculate the next Position and Direcion
      ;;
      (define (move)
        (if (not graph)
            (set! graph (maze 'to-graph)))
        (let* ((pacman (maze 'get-the-pacman))
               (target (pacman 'get-position))
               (pair (if (eq? state 'dead)
                         (ai-fun graph position direction home state)
                         (ai-fun graph position direction target state)))
               (next-pos (car pair))
               (next-dir (cdr pair))
               (curr-cell (maze 'get position))
               (next-cell (maze 'get next-pos)))
          (set! position next-pos)
          (set! direction next-dir)
          (curr-cell 'remove-ghost self)
          (next-cell 'add-ghost self)))
      ;;
      ;; dispatched called self
      ;;
      (define (self m . args)
        (cond
          ((eq? m 'get-position)   (apply get-position args))
          ((eq? m 'get-type)       (apply get-type args))
          ((eq? m 'get-maze)       (apply get-maze args))
          ((eq? m 'get-direction)  (apply get-direction args))
          ((eq? m 'set-direction!) (apply set-direction! args))
          ((eq? m 'get-state)      (apply get-state args))
          ((eq? m 'set-state!)     (apply set-state! args))
          ((eq? m 'set-ai-fun!)    (apply set-ai-fun! args))
          ((eq? m 'display)        (apply _display args))
          ((eq? m 'go-home)        (apply go-home args))
          ((eq? m 'move)           (apply move args))
          (else (error "PACMAN-PACMAN -- unknown message:" m))))
      self))
  
)