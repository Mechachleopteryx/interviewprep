#lang racket

(require "stack.rkt")

(define (make-spindle cls%)
  (class cls%
    (super-new)
    (init index)
    (define spindle-index index)
    (define/public (get-index)
      spindle-index)))

(define (make-towers n)
  (let ([left (new (make-spindle ordered-stack%)
                   [index 0])]
        [middle (new (make-spindle ordered-stack%)
                     [index 1])]
        [right (new (make-spindle ordered-stack%)
                    [index 2])])
    (for ([i (in-range n 0 -1)])
      (send left push i))
    (values left middle right)))

(define (print-towers . towers)
  (println (sort towers
                 (λ (x y)
                   (< (send x get-index)
                      (send y get-index))))))

(define (move-disk source target spare)
  (define disk (send source pop))
  (send target push disk)
  (print-towers source target spare))

(define (move-tower-of-height source target spare height)
  (cond [(= 0 height) (void)]
        [(= 1 height) (move-disk source target spare)]
        [else (begin (move-tower-of-height source
                                           spare
                                           target
                                           (sub1 height))
                     (move-disk source target spare)
                     (move-tower-of-height spare
                                           target
                                           source
                                           (sub1 height)))]))

(define (move-tower source target spare)
  (move-tower-of-height source
                        target
                        spare
                        (send source size)))

(provide make-towers
         move-tower)
