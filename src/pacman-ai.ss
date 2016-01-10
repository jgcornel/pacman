;;;
;;; pacman-ai.ss
;;; 
;;; Jan Cornelis
;;; 2007-2008
;;;
;;; This module provides functions that create functions to move the Ghosts
;;; The returned functions all have the following signature;
;;;
;;;  function : Graph Position Direction Position symbol
;;;
;;; Where the first Position is the Position of the Ghost, the second 
;;; Position is the Position of the target, Direction is the Direction of
;;; the Ghost and symbol represents the state of the host.
;;; 
;;;
;;; *** The following functions are provided ***
;;;
;;; make-random-mover
;;; ----------------- 
;;; Returns a function that will cause a Ghost to move at random when he is
;;; in the weak or strong state. 
;;;
;;; make-mts-mover
;;; --------------
;;; Returns a function that will mimic the Moving Target Search algorithm
;;; as explained in:
;;;
;;; "T. Ishida and R. E. Korf, Moving Target Search, IJCAI-91, pp. 204-210,
;;;  1991"
;;;
;;; make-jc-mover
;;; -------------
;;; Returns a function that will cause a ghost in the weak or strong state
;;;    - to turn back only if there is no other possibility.
;;;    - to choose the cell for which the manhattan-distance to Pacman's
;;;      cell is smallest or biggest.
;;; 
;;; The reason why this module provides functions that return the actual
;;; functions that will be used to move the Ghosts is simple. For some
;;; algorithms it is necessary to remember things from the previous runs,
;;; and for all algorithms it greatly simplifies things if this can be done.
;;; Therefore these things are kepts as local variables of the creating
;;; functions, thus allowing them to be updated and accessed by the
;;; returned function. I guess this is called a closure.
;;;
;;; Every function is defined in terms of the general make-ai-mover
;;; this function expects two functions as it's arguments; one function
;;; will be used to analyze the movement of the target (and possibly update
;;; heuristic values), while the other is used to choose the best move for
;;; the Ghost (and possibly update heuristic values).
;;;
;;; It should be noted that the target is not necessarily Pacman. When a
;;; Ghost is dead the target will be it's home. Every algorithm will use an
;;; a* graph search to calculate the shortest route home. The a* graph
;;; search was based upon the book:
;;;
;;; "Russell, Stuart  J.; Norvig, Peter. Artificial Intelligence: A Modern
;;;  Approach (2nd Edition). Prentice Hall"
;;;
;;; The a*-search function is exported for testing purposes.
;;;________________________________________________________________________

(module pacman-ai mzscheme
  
  (require (lib "list.ss")
           "pacman-util.ss"
           "pacman-heap.ss")
  
  (provide make-ai-mover      ; function function       -> function
           make-random-mover  ;                         -> function           
           make-mts-mover     ;                         -> function
           make-jc-mover      ;                         -> function
           a*-search)         ; Graph Position Position -> list
    
  ;;; make-ai-mover : Graph function function -> function
  ;;;______________________________________________________________________
  (define (make-ai-mover pacman-mover-function! ghost-mover-function!)
    (let ((previous-target #f)
          (previous-state  #f)
          (positions      '())
          (heuristics (make-table)))
      (lambda (graph position direction current-target current-state)
        (let ((next-position #f))
          ;;
          ;; If the Previous target was Pacman and Pacman has moved the
          ;; movement can be analyzed and used to update heuristics
          ;;
          (if (and previous-target
                   (not (position=? previous-target current-target))
                   (or (eq? previous-state 'weak)
                       (eq? previous-state 'strong)))
              (pacman-mover-function! graph 
                                      position 
                                      previous-target
                                      current-target
                                    heuristics))
          ;;
          ;; If the current state is 'weak or 'strong the next position
          ;; needs to be calculated based upon the chosen strategy which
          ;; is implemented by the function ghost-mover-function!
          ;;
          (if (or (eq? current-state 'weak)
                  (eq? current-state 'strong))           
              (set! next-position (ghost-mover-function! graph
                                                         position
                                                         direction
                                                         current-target
                                                         current-state
                                                         heuristics))
              ;;
              ;; If the current state is 'dead the next position will be
              ;; taken from a list of positions that is first calculated 
              ;; using the ubiquitous a* search
              ;;
              (begin
                (if (not (eq? previous-state 'dead))
                    (set! positions 
                          (a*-search graph position current-target)))
                (set! next-position (car positions))
                (set! positions (cdr positions))))
          (set! previous-state current-state)
          (set! previous-target current-target)
          (cons next-position (direction-from-to position next-position))))))
  
  ;;; make-random-mover : -> function
  ;;;______________________________________________________________________
  (define (make-random-mover)
    (make-ai-mover ignore-pacman move-ad-random))
  
  (define (ignore-pacman graph pos prev-target curr-target heuristics)
    void)
  
  (define (move-ad-random graph pos dir curr-target curr-state heuristics)
    (let* ((all-neighbours (graph-position-ref graph pos))
           (prev-pos (position-in-direction pos (direction-opposite dir)))
           (new-neighbours (remove-from prev-pos all-neighbours))
           (milliseconds (abs (current-milliseconds))))
      (if (null? new-neighbours)
          prev-pos
          (let* ((new-neighbours-length (length new-neighbours))
                 (pseudo-random-index (remainder milliseconds
                                                 new-neighbours-length)))
            (get-at-index new-neighbours pseudo-random-index)))))
  
  ;;; make-mts-mover : -> function
  ;;;______________________________________________________________________
  (define (make-mts-mover)
    (make-ai-mover analyze-pacman! move-as-mts!))
  
  (define (analyze-pacman! graph pos prev-target curr-target heuristics)
    (let* ((hxy  (get-heuristic heuristics pos prev-target))
           (hxyy (get-heuristic heuristics pos curr-target))
           (delta-y (get-heuristic heuristics prev-target curr-target)))
      (if (> (- hxyy delta-y) hxy)
          (table-insert! heuristics
                         pos
                         prev-target
                         (- hxyy delta-y)))))
  
  (define (move-as-mts! graph pos dir curr-target curr-state heuristics)
    (let* ((hxy (get-heuristic heuristics pos curr-target))
           (neighbours (graph-position-ref graph pos))
           (xx-max-pair (extract-max neighbours curr-target heuristics))
           (xx-min-pair (extract-min neighbours curr-target heuristics))
           (xx-min-value (cdr xx-min-pair)))
      (if (> (+ xx-min-value 1) hxy)
          (table-insert! heuristics
                         pos
                         curr-target
                         (+ xx-min-value 1)))
      (if (eq? curr-state 'strong)
          (car xx-min-pair)
          (car xx-max-pair))))
  
  ;;; make-jc-mover : -> function
  ;;;______________________________________________________________________
  (define (make-jc-mover)
    (make-ai-mover ignore-pacman move-as-jc))
  
  (define (move-as-jc graph pos dir curr-target curr-state heuristics)
    (let* ((all-neighbours (graph-position-ref graph pos))
           (prev-pos (position-in-direction pos (direction-opposite dir)))
           (new-neighbours (remove-from prev-pos all-neighbours)))
      (if (null? new-neighbours)
          prev-pos
          (let ((xx-max-pair (extract-max new-neighbours curr-target heuristics))
                (xx-min-pair (extract-min new-neighbours curr-target heuristics)))
            (if (eq? curr-state 'strong)
                (car xx-min-pair)
                (car xx-max-pair))))))
                            
  ;;; Helpers
  ;;;______________________________________________________________________
  
  ;; get-heuristic : Table Position Position -> number
  ;; Return the estimated shortest distance between the given Positions
  ;;_______________________________________________________________________
  (define (get-heuristic table from to)
    (let ((distance (table-lookup table from to)))
      (if distance
          distance
          (position-manhattan-distance from to))))
  
  ;;; Extractors
  ;;;______________________________________________________________________
  
  ;;; everybody is allowed to have some fun sometimes ...
  
  (define (extract alist fun op?)
    (car
     (sort
      (map (lambda (e) (cons e (fun e))) alist)
      (lambda (e1 e2) (op? (cdr e1) (cdr e2))))))
  
  ;;; extract-min : list Position Table -> pair
  ;;; Search a Position in positions for which the estimated distance to
  ;;; the given Position is minimal. Return a (Position . distance) pair.
  ;;;______________________________________________________________________
  (define (extract-min positions position heuristics)
    (extract 
     positions
     (lambda (p) 
       (get-heuristic heuristics p  position)) <))

  ;;; extract-max : list Position Table -> pair
  ;;; Search a Position in positions for which the estimated distance to
  ;;; the given Position is maximal. Return a (Position . distance) pair.
  ;;;______________________________________________________________________  
  (define (extract-max positions position heuristics)
    (extract 
     positions
     (lambda (p) 
       (get-heuristic heuristics p  position)) >)) 
  
  ;;; get-at-index : list number -> object
  ;;; Return the index-th element of a-list
  ;;;______________________________________________________________________
  (define (get-at-index a-list index)
    (vector-ref (list->vector a-list) index))             
  
  ;;; A* graph-search
  ;;;______________________________________________________________________
  
  ;;; Helper Node ADT
  ;;;_______________________________________________________________________
  
  (define (make-node pos parent g-cost h-cost)
    (list pos parent g-cost h-cost))
  
  (define (node-position-ref node)
    (car node))
  
  (define (node-parent-ref node)
    (cadr node))
  
  (define (node-g-cost-ref node)
    (caddr node))
  
  (define (node-h-cost-ref node)
    (cadddr node))
  
  (define (node-f-cost-ref node)
    (+ (node-g-cost-ref node) (node-h-cost-ref node)))
  
  (define (node<? node1 node2)
    (< (node-f-cost-ref node1) (node-f-cost-ref node2)))
  
  ;;; a*-search : Graph Position Position -> list
  ;;; Return a list of Positions that form a Path between the given
  ;;; Positions. The goal Position is included in the list but the source
  ;;; Position is not.
  ;;;______________________________________________________________________
  (define (a*-search graph from to)
    (let* ((rows (graph-number-of-rows graph))
           (cols (graph-number-of-cols graph))
           (cells (* rows cols))
           (heap (make-heap cells node<?))
           (closed-positions '())
           (root-node 
            (make-node from #f 
                            0 
                            (position-manhattan-distance from to))))
      ;;
      ;; expand-node : Node -> list
      ;; returns a list of Nodes of Positions that are adjacent to the 
      ;; Node's position and that have not yet been encountered
      ;;___________________________________________________________________
      (define (expand-node node)
        (let* ((pos (node-position-ref node))
               (g-cost (node-g-cost-ref node))
               (positions (graph-position-ref graph pos)))
          (set! closed-positions (cons pos closed-positions))
          (map
           (lambda (p)
             (make-node p
                        node
                        (+ g-cost 1)
                        (position-manhattan-distance p to)))
           (filter
            (lambda (p)
              (not (member p closed-positions)))
            positions))))
      ;;
      ;; search : -> Node
      ;; Carry out the actual search and returns a Node that contains
      ;; the goal Position
      ;;___________________________________________________________________
      (define (search)
        (if (heap 'empty?)
            (error "A*-SEARCH -- a route could not be found")
            (let ((node (heap 'top)))
              (heap 'pop!)
              (if (position=? (node-position-ref node) to)
                  node
                  (begin
                    (for-each
                     (lambda (n)
                       (heap 'add! n))
                     (expand-node node))
                    (search))))))
      ;;
      ;; node->positions : Node -> list
      ;; Return a list of Positions. These Positions correspond to the
      ;; path that should be followed to reach the goal Position
      ;;___________________________________________________________________
      (define (node->positions node)
        (define (walk-up a-l curr-node)
          (let ((curr-pos (node-position-ref curr-node))
                (next-node (node-parent-ref curr-node)))
            (if (position=? curr-pos from)
                a-l
                (walk-up (cons curr-pos a-l) next-node))))
        (walk-up '() node))
      (heap 'add! root-node)
      (node->positions (search))))
)
