#lang racket

(require "vector-swap.rkt")

(define (sort my-list
              #:ascending? [ascending? #t])
  (let ([vec (list->vector my-list)]
        [compare? (if ascending?
                      >
                      <)])
    (for* ([i (in-range (- (vector-length vec)
                           1))]
           [j (in-range (+ i 1) (vector-length vec))])
      (when (compare? (vector-ref vec i)
                      (vector-ref vec j))
        (swap! vec i j)))
    (vector->list vec)))

(provide sort)
