#lang racket

(require "vector-swap.rkt")

(define (sort my-list
              #:ascending? [ascending? #t])
  (let ([vec (list->vector my-list)]
        [compare? (if ascending?
                      >
                      <)])
    (define (bubble-up)
      (let ([swapped #f])
        (for* ([i (in-range (- (vector-length vec)
                               1))])
          (let* ([j (+ i 1)]
                 [current-item (vector-ref vec i)]
                 [next-item (vector-ref vec j)])
            (when (compare? current-item
                            next-item)
              (swap! vec i j)
              (set! swapped #t))))
        swapped))
    (let loop ([swapped #t])
      (when swapped
        (loop (bubble-up))))
    (vector->list vec)))

(provide sort)
