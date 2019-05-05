#lang racket

(require "graph.rkt")
(require "queue.rkt")

(define (preprocess graph)
  (let ([node-queue (new queue%)]
        [edge-counts (make-hash)])
    (for ([node (in-dict-values (send graph get-nodes))])
      (define n-parents
        (length
         (set->list
          (send node get-in-neighbors))))
      (define nid (send node get-id))
      (hash-set! edge-counts
                 nid
                 n-parents)
      (when (= n-parents
               0)
        (send node-queue enqueue node)))
    (values node-queue edge-counts)))

;; normal list-based approach
(define (topological-sort graph)
  (let-values ([(node-queue edge-counts) (preprocess graph)])
    (let loop ([result null])
      (define node (send node-queue dequeue))
      (if (void? node)
          (if (> (for/sum ([i (in-dict-values edge-counts)]) i)
                 0)
              (error "Graph contains a cycle!")
              result)
          (begin
            (for ([child (in-set (send node get-out-neighbors))])
              (define cid (send child get-id))
              (define n-edges
                (hash-ref edge-counts cid))
              (hash-set! edge-counts
                         cid
                         (sub1 n-edges))
              (when (= (hash-ref edge-counts cid)
                       0)
                (send node-queue enqueue child)))
            (loop (append result
                          (list node))))))))

;; stream-based approach
(define (in-topological-sort graph)
  (let-values ([(node-queue edge-counts) (preprocess graph)])
    (let loop ()
      (define node (send node-queue dequeue))
      (if (void? node)
          (if (> (for/sum ([i (in-dict-values edge-counts)]) i)
                 0)
              (error "Graph contains a cycle!")
              empty-stream)
          (begin
            (for ([child (in-set (send node get-out-neighbors))])
              (define cid (send child get-id))
              (define n-edges
                (hash-ref edge-counts cid))
              (hash-set! edge-counts
                         cid
                         (sub1 n-edges))
              (when (= (hash-ref edge-counts cid)
                       0)
                (send node-queue enqueue child)))
            (stream-cons node (loop)))))))

(provide topological-sort)
