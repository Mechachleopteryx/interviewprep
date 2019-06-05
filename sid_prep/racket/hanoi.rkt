#lang racket

(require "stack.rkt")

(define (make-towers n)
  (let ([left (new ordered-stack%)]
        [middle (new ordered-stack%)]
        [right (new ordered-stack%)])
    (for ([i (in-range n 0 -1)])
      (send left push i))
    (values left middle right)))

(define (move-disk source target)
  (define disk (send source pop))
  (send target push disk))

(define (move-tower-of-height source target spare height)
  (cond [(= 0 height) (void)]
        [(= 1 height) (move-disk source target)]
        [else (begin (move-tower-of-height source
                                           spare
                                           target
                                           (sub1 height))
                     (move-disk source target)
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
