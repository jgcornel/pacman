;;;
;;; pacman-heap
;;;
;;; Jan Cornelis
;;; 2007-2008
;;;
;;; This module provides a Heap ADT.
;;; It is a helper module for pacman-event.ss and pacman-ai.ss
;;;
;;; Heap
;;; ----
;;; A Heap represents a heap and provides the expected methods.
;;; The implementation is based upon chapter 7 of the well known book
;;; 
;;; "Cormen, Thomas H.; Leiserson, Charles E.; Rivest, Ronald L.; Stein,
;;;  Clifford. Introduction to Algorithms, second edition, MIT Press and
;;;  McGraw-Hill"
;;;
;;; We refer to a book for a detailed overview of the algorithms used
;;;________________________________________________________________________

(module pacman-heap mzscheme
  
  (provide make-heap)
  
  ;; Heap ADT
  ;;
  ;; Messages:
  ;;
  ;;  'empty?    :          -> boolean
  ;;  'top       :          -> object
  ;;  'pop!      :          -> void
  ;;  'add!      : object   -> void
  ;;  'remove!   : object   -> void
  ;;  'for-each  : function -> void
  ;;_______________________________________________________________________
  (define (make-heap maximum-size compare-function)
    (let ((the-heap (make-vector (+ maximum-size 1)))
          (size 0))
      ;;
      ;; helpers
      ;;
      (define (_left index)
        (* 2 index))
      (define (_right index)
        (+ (* 2 index) 1))
      (define (_parent index)
        (quotient index 2))
      (define (_swap index1 index2)
        (let ((elt1 (vector-ref the-heap index1))
              (elt2 (vector-ref the-heap index2)))
          (vector-set! the-heap index1 elt2)
          (vector-set! the-heap index2 elt1)))
      (define (_heapify index)
        (if (< index size)
            (let ((left (_left index))
                  (right (_right index))
                  (first index))
              (if (and (<= left size)
                       (compare-function 
                        (vector-ref the-heap left)
                        (vector-ref the-heap first)))
                  (set! first left))
              (if (and (<= right size)
                       (compare-function 
                        (vector-ref the-heap right)
                        (vector-ref the-heap first)))
                  (set! first right))
              (if (not (= first index))
                  (begin
                    (_swap first index)
                    (_heapify first))))))
      (define (_bubble-up index)
        (if (> index 1)
            (let* ((elt (vector-ref the-heap index))
                   (parent (_parent index))
                   (parent-elt (vector-ref the-heap parent)))
              (if (compare-function elt parent-elt)
                  (begin
                    (_swap parent index)
                    (_bubble-up parent))))))
      ;;
      ;; interfaces
      ;;
      (define (empty?)
        (= size 0))
      (define (top)
        (if (> size 0)
            (vector-ref the-heap 1)
            (error "HEAP -- underflow")))
      (define (pop!)
        (if (> size 0)
            (begin
              (vector-set! the-heap 1 (vector-ref the-heap size))
              (set! size (- size 1))
              (_heapify 1))
            (error "HEAP -- underflow")))
      (define (add! elt)
        (if (< size maximum-size)
            (begin
              (set! size (+ size 1))
              (vector-set! the-heap size elt)
              (_bubble-up size))
            (error "HEAP -- overflow")))
      (define (remove! elt)
        (define (iter index)
          (if (<= index size)
              (if (eq? elt (vector-ref the-heap index))
                  (begin
                    (vector-set! the-heap index (vector-ref the-heap size))
                    (set! size (- size 1))
                    (_heapify index))
                  (iter (+ index 1)))))
          (iter 1))      
      (define (_for-each fun)
        (define (iter index)
          (if (<= index size)
              (begin
                (fun (vector-ref the-heap index))
                (iter (+ index 1)))))
        (iter 1))
      ;;
      ;; dispatch
      ;;
      (define (dispatch m . args)
        (cond
          ((eq? m 'empty?)   (apply empty? args))
          ((eq? m 'top)      (apply top args))
          ((eq? m 'pop!)     (apply pop! args))
          ((eq? m 'add!)     (apply add! args)) 
          ((eq? m 'remove!)  (apply remove! args))
          ((eq? m 'for-each) (apply _for-each args))
          (else (error "HEAP -- unknown message:" m))))
      dispatch))
  
  )