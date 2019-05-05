#lang racket

(define ∈ set-member?)

(define (contains-cycle? my-list)
  (let ([visited-set (mutable-set)])
    (define (deja-vu? node)
      (∈ visited-set node))
    (define (loop current-node)
      (if (deja-vu? current-node)
          #t
          (begin (set-add! visited-set current-node)
                 (if (equal? (mcdr current-node)
                             null)
                     #f
                     (loop (mcdr current-node))))))
    (let ([head (send my-list show)])
      (loop head))))

(provide contains-cycle?)
