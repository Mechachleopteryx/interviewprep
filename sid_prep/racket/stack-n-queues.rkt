#lang racket

(require "stack.rkt"
         "queue.rkt")

;; queue implemented using two stacks
(define queue-on-stacks%
  (class queue%
    (super-new)

    (field [main (new stack%)])
    (field [spare (new stack%)])

    (define/override (enqueue elem)
      (let loop ()
        (when (not (send main empty?))
          (send spare push (send main pop))
          (loop)))
      (send main push elem)
      (let loop ()
        (when (not (send spare empty?))
          (send main push (send spare pop))
          (loop))))

    (define/override (dequeue)
      (send main pop))

    (define/override (peek)
      (send main peek))))

;; stack implemented using two queues
(define stack-on-queues%
  (class stack%
    (super-new)

    (field [left (new queue%)])
    (field [right (new queue%)])
    (field [front left])

    (define/override (push elem)
      (let ([back (if (eq? front left)
                      right
                      left)])
        (send back enqueue elem)
        (let loop ()
          (when (not (send front empty?))
            (send back
                  enqueue (send front
                                dequeue))
            (loop)))
        (set! front back)))

    (define/override (pop)
      (send front dequeue))

    (define/override (peek)
      (send front peek))))

;; stack implemented using single queue
(define stack-on-queue%
  (class stack%
    (super-new)

    (field [queue (new queue%)])

    (define/override (push elem)
      (send queue enqueue elem)
      (let loop ()
        (when (not (equal? (send queue peek)
                           elem)) ; assume unique elements
          (send queue
                enqueue (send queue
                              dequeue))
          (loop))))

    (define/override (pop)
      (send queue dequeue))

    (define/override (peek)
      (send queue peek))))
