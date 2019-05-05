#lang racket/base

(require rackunit
         "topological-sort.rkt"
         "graph-test-util.rkt")

(test-case
    "Preprocess test"
  (let ([g (generate-graph 'empty)])
    (let-values ([(node-queue edge-counts) (preprocess g)])
      (check-true (send node-queue empty?))))
  (let ([g (generate-graph 'unconnected)])
    (let-values ([(node-queue edge-counts) (preprocess g)])
      (check-equal? (list->set (send node-queue as-list))
                    (list->set (dict-values (send g get-nodes))))))
  (let ([g (generate-graph 'linear)])
    (let-values ([(node-queue edge-counts) (preprocess g)])
      (check-equal? (length (send node-queue as-list))
                    1)))
  (let ([g (generate-graph 'tree)])
    (let-values ([(node-queue edge-counts) (preprocess g)])
      (check-equal? (length (send node-queue as-list))
                    1)))
  (let ([g (generate-graph 'components)])
    (let-values ([(node-queue edge-counts) (preprocess g)])
      (check-equal? (length (send node-queue as-list))
                    2)))
  (let ([g (generate-graph 'dag)])
    (let-values ([(node-queue edge-counts) (preprocess g)])
      (check-equal? (length (send node-queue as-list))
                    2)))
  (let ([g (generate-graph 'cyclic)])
    (let-values ([(node-queue edge-counts) (preprocess g)])
      (check-equal? (length (send node-queue as-list))
                    0))))

(test-case
    "Topological sort test"
  (let ([g (generate-graph 'empty)])
    (let ([result (topological-sort g)])
      (check-pred null? result)))
  (let ([g (generate-graph 'unconnected)])
    (let ([result (topological-sort g)])
      (check-equal? (list->set result)
                    (list->set (dict-values (send g get-nodes))))))
  (let ([g (generate-graph 'linear)])
    (let ([result (topological-sort g)])
      (check-equal? (map (λ (n) (send n get-id))
                         result)
                    '(0 1 2))))
  (let ([g (generate-graph 'tree)])
    (let ([result (map (λ (n) (send n get-id))
                       (topological-sort g))])
      (check <
             (index-of result 0)
             (index-of result 1))
      (check <
             (index-of result 2)
             (index-of result 5))))
  (let ([g (generate-graph 'components)])
    (let ([result (map (λ (n) (send n get-id))
                       (topological-sort g))])
      (check <
             (index-of result 0)
             (index-of result 1))
      (check <
             (index-of result 3)
             (index-of result 4))))
  (let ([g (generate-graph 'dag)])
    (let ([result (map (λ (n) (send n get-id))
                       (topological-sort g))])
      (check <
             (index-of result 0)
             (index-of result 1))
      (check <
             (index-of result 2)
             (index-of result 4))))
  (let ([g (generate-graph 'cyclic)])
    (check-exn exn:fail?
               (λ () (topological-sort g)))))
