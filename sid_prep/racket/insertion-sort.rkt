#lang racket

(require "vector-swap.rkt")

(define (sort my-list)
  (let ([vec (list->vector my-list)])
    (let loop ([unsorted-index 0])
      (if (< unsorted-index
             (vector-length vec))
          (let reverse-loop ([elem-index unsorted-index])
            (if (and (> elem-index
                        0)
                     (< (vector-ref vec
                                    elem-index)
                        (vector-ref vec
                                    (sub1 elem-index))))
                (begin (swap! vec
                              elem-index
                              (sub1 elem-index))
                       (reverse-loop (sub1 elem-index)))
                (loop (add1 unsorted-index))))
          vec))
    (vector->list vec)))

(provide sort)
