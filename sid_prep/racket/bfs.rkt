#lang racket

(require "queue.rkt")

(define (search graph goal?)
  (let ([node-queue (new queue%)]
        [start-node
         (first
          (sequence->list
           (in-dict-values
            (send graph get-nodes))))])
    (send node-queue enqueue start-node)
    (search-nodes node-queue goal?)))

(define (search-nodes node-queue
                      goal?
                      [visited (mutable-set)])
  (let loop ()
    (when (not (send node-queue empty?))
      (let ([current-node (send node-queue dequeue)])
        (if (goal? (send current-node get-value))
            current-node
            (begin
              (set-add! visited current-node)
              (let ([unvisited-neighbors
                     (for/list ([n (in-set (send current-node
                                                 get-neighbors))]
                                #:when (not (set-member? visited n)))
                       n)])
                (send node-queue enqueue-all unvisited-neighbors))
              (loop)))))))
