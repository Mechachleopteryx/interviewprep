#lang racket

(require "graph.rkt")

(define (search graph goal?)
  (search-node (first
                (sequence->list
                 (in-dict-values
                  (send graph
                        get-nodes))))
               goal?))

(define (search-node current-node
                     goal?
                     [visited (mutable-set)])
  (if (goal? (send current-node get-value))
      current-node
      (begin
        (set-add! visited current-node)
        (let loop ([neighbors (set->list (send current-node
                                               get-neighbors))])
          (when (not (null? neighbors))
            (let ([neighbor (first neighbors)])
              (if (set-member? visited neighbor)
                  (loop (rest neighbors))
                  (let ([result (search-node neighbor
                                             goal?
                                             visited)])
                    (if (void? result)
                        (loop (rest neighbors))
                        result)))))))))

(provide search)
