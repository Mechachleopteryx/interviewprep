#lang racket/base

(require rackunit
         "graph-test-util.rkt"
         "dfs.rkt")

(test-case
    "DFS test"
  (let ([g (generate-graph 'dag)])
    (check-equal? (search g (λ (v) (= v 5)))
                  (send g get-node 4)))

  (let* ([g (generate-graph 'dag)]
         [found-node (search g (λ (v) (< v 5)))])
    (check <
           (send found-node get-value)
           5))

  (let* ([g (generate-graph 'dag)]
         [found-node (search g (λ (v) (> v 5)))])
    (check >
           (send found-node get-value)
           5))

  (let* ([g (generate-graph 'dag)]
         [found-node (search g (λ (v) (< v 0)))])
    (check-pred void? found-node)))
