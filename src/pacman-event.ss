;;;
;;; pacman-event.ss
;;;
;;; Jan Cornelis
;;; 2007-2008
;;;
;;; This module provides two ADTs that are tightly coupled: the Event and
;;; the Event-Queue ADT.
;;;
;;; Event
;;; -----
;;; An Event represents something that should happen at a given instant in
;;; time. Therefore a procedure and an object on which the procedure has to
;;; be applied should be given. To make it possible to schedule this event
;;; an Event-Queue should also be given, as well as an interval to wait 
;;; before executing the event. It is possible to schedule an event only
;;; once or to make it repeat ad infinitum. 
;;;
;;; Event-Queue
;;; -----------
;;; An Event-Queue should be used to schedule Events. Methods are provided
;;; to add and remove Events to the queue, to execute all Events that are
;;; pending and to countdown the logical times of the Events
;;;
;;; Our notion of time is not a physical one: every Event will have an
;;; attribute counter that needs to be decreased to simulate the progress
;;; of time. When an Event's counter reaches zero, it should be executed.
;;;
;;; The Heap ADT provided by "pacman-heap.ss" was used to implement the
;;; Event-Queue
;;;________________________________________________________________________

(module pacman-event mzscheme
  
  (require "pacman-heap.ss")
  
  (provide make-event        ; procedure object number Event-Queue boolean -> Event
           make-event-queue) ; number                                      -> Event-Queue
  
  ;; Event ADT
  ;;
  ;; Messages:
  ;; 
  ;;  'start              :  -> void
  ;;  'stop               :  -> void
  ;;  'execute            :  -> void
  ;;  'only-once?         :  -> boolean
  ;;  'get-counter        :  -> number
  ;;  'decrement-counter! :  -> void
  ;;_______________________________________________________________________
  (define (make-event function object interval event-queue only-once?)
    (let ((counter interval))
      ;;
      ;; Interfaces
      ;;
      (define (start)
        (set! counter interval)
        (event-queue 'add! self))
      (define (stop)
        (event-queue 'remove! self))
      (define (execute)
        (function object)
        void)
      (define (_only-once?)
        only-once?)
      (define (get-counter)
        counter)
      (define (decrement-counter!)
        (set! counter (- counter 1)))
      ;;
      ;; dispatch called self
      ;;
      (define (self m . args)
        (cond 
          ((eq? m 'start)              (apply start args))
          ((eq? m 'stop)               (apply stop args))
          ((eq? m 'execute)            (apply execute args))
          ((eq? m 'only-once?)         (apply _only-once? args))
          ((eq? m 'get-counter)        (apply get-counter args))
          ((eq? m 'decrement-counter!) (apply decrement-counter! args))
          (else (error "PACMAN-EVENT -- unknown message:" m))))
      self))
  
  ;; Event-Queue ADT
  ;;
  ;; Messages:
  ;;
  ;;  'add!              : Event -> void
  ;;  'remove!           : Event -> void
  ;;  'countdown         :       -> void
  ;;  'handle-due-events :       -> void
  ;;_______________________________________________________________________
  (define (make-event-queue maximum-size)
    (let* ((cmp-fun (lambda (e1 e2) 
                  (< (e1 'get-counter) (e2 'get-counter))))
           (the-heap (make-heap maximum-size cmp-fun)))
      ;;
      ;; Interfaces
      ;;
      (define (add! event)
        (the-heap 'add! event))
      (define (remove! event)
        (the-heap 'remove! event))
      ;;
      ;; Advance time for the contained Events
      ;;
      (define (countdown)
        (the-heap 
         'for-each
         (lambda (e)
           (e 'decrement-counter!))))
      ;;
      ;; Execute all Events having a zero counter
      ;;
      (define (handle-due-events)
        (let ((top-evt (the-heap 'top)))
          (if (<= (top-evt 'get-counter) 0)
              (begin
                (the-heap 'pop!)
                (top-evt 'execute)
                (if (not (top-evt 'only-once?))
                    (top-evt 'start))
                (handle-due-events)))))
      ;;
      ;; dispatch
      ;;
      (define (dispatch m . args)
        (cond 
          ((eq? m 'add!)              (apply add! args))
          ((eq? m 'remove!)           (apply remove! args))
          ((eq? m 'countdown)         (apply countdown args))
          ((eq? m 'handle-due-events) (apply handle-due-events args))
          (else (error "PACMAN-EVENT-QUEUE -- unknown message: m"))))
      dispatch))
  
  )
