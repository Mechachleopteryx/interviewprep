#lang racket

(define ∈ set-member?)

(define (contains-cycle? my-list)
  ; uses scheme-style (m)cons list as a linked list
  (let ([visited-set (mutable-set)])
    (define (deja-vu? node)
      (∈ visited-set node))
    (let loop ([current-node my-list])
      (if (deja-vu? current-node)
          #t
          (let ([next-node (mcdr current-node)])
            (set-add! visited-set current-node)
            (if (null? next-node)
                #f
                (loop next-node)))))))

(provide contains-cycle?)
